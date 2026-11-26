#!/usr/bin/env python3
"""
Run backend-tv in riscv64 mode on every function of every .ll file in the
LLVM Transforms tests and in the RV64 CodeGen tests, one invocation per
function, collecting the logs into a single directory.

Output layout (under --out):

  logs/<suite>/<path of .ll relative to the suite>/<NNNN>-<function>.log
      One log per backend-tv invocation. The first lines of each log are a
      header giving the source .ll file, function name, and command line,
      so every log can be traced back to its input.
  manifest.jsonl
      One JSON record per invocation: file, function, log, rc, status,
      seconds, and the counts from backend-tv's summary.
  summary.txt
      Totals by status, written at the end of the run.

Re-running with the same --out skips invocations whose log already exists,
so an interrupted sweep can be resumed.
"""

import argparse
import json
import os
import re
import resource
import signal
import subprocess
import sys
import threading
import time
from collections import Counter
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]

# `define` lines, possibly preceded by linkage etc. on the same line. The
# function name is either a bare identifier or a quoted string.
DEFINE_RE = re.compile(
    r'^\s*define\b[^@]*@("(?:[^"\\]|\\.)*"|[-a-zA-Z$._0-9]+)\s*\(', re.M)
RV64_RUN_RE = re.compile(r'^\s*;.*RUN:.*riscv64', re.M)
SUMMARY_RE = {
    'correct': re.compile(r'^\s*(\d+) correct transformations', re.M),
    'incorrect': re.compile(r'^\s*(\d+) incorrect transformations', re.M),
    'failed_to_prove': re.compile(r'^\s*(\d+) failed-to-prove transformations', re.M),
    'alive_errors': re.compile(r'^\s*(\d+) Alive2 errors', re.M),
}
DETAIL_RE = re.compile(r'^(ERROR:.*|.*Assertion .* failed.*|LLVM ERROR:.*)$', re.M)


def functions_in(path):
    try:
        text = path.read_text(errors='replace')
    except OSError:
        return []
    names = []
    seen = set()
    for m in DEFINE_RE.finditer(text):
        name = m.group(1)
        if name.startswith('"'):
            name = name[1:-1]
        if name not in seen:
            seen.add(name)
            names.append(name)
    return names


def collect_inputs(llvm, suites):
    """Yield (suite, relpath, abspath) for each input file."""
    if 'transforms' in suites:
        root = llvm / 'test' / 'Transforms'
        for p in sorted(root.rglob('*.ll')):
            yield 'Transforms', p.relative_to(root), p
    if 'riscv' in suites:
        root = llvm / 'test' / 'CodeGen' / 'RISCV'
        for p in sorted(root.rglob('*.ll')):
            try:
                if not RV64_RUN_RE.search(p.read_text(errors='replace')):
                    continue
            except OSError:
                continue
            yield 'CodeGen-RISCV', p.relative_to(root), p


def safe_name(fn, maxlen=80):
    s = re.sub(r'[^A-Za-z0-9._-]', '_', fn)
    return s[:maxlen] or '_'


def classify(rc, output, timed_out):
    counts = {k: (int(m.group(1)) if (m := r.search(output)) else None)
              for k, r in SUMMARY_RE.items()}
    if timed_out:
        status = 'timeout'
    elif counts['incorrect']:
        status = 'incorrect'
    elif counts['alive_errors']:
        status = 'alive-error'
    elif counts['failed_to_prove']:
        status = 'failed-to-prove'
    elif counts['correct']:
        status = 'correct'
    elif rc < 0:
        status = f'signal-{-rc}'
    elif rc != 0:
        status = 'error'
    else:
        status = 'no-summary'
    m = DETAIL_RE.search(output)
    return status, counts, (m.group(1).strip()[:300] if m else None)


def run_one(job, args):
    suite, rel, path, idx, fn, log = job
    cmd = [str(args.backend_tv), '-backend=riscv64', '-always-verify',
           f'--fn={fn}', *args.extra, str(path)]

    def limits():
        if args.mem_limit:
            b = args.mem_limit * 1024 * 1024
            resource.setrlimit(resource.RLIMIT_AS, (b, b))
        resource.setrlimit(resource.RLIMIT_CORE, (0, 0))

    t0 = time.monotonic()
    timed_out = False
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                         stdin=subprocess.DEVNULL, cwd=args.workdir,
                         start_new_session=True, preexec_fn=limits)
    try:
        out, _ = p.communicate(timeout=args.timeout)
    except subprocess.TimeoutExpired:
        timed_out = True
        try:
            os.killpg(p.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        out, _ = p.communicate()
    secs = time.monotonic() - t0
    output = out.decode(errors='replace')
    status, counts, detail = classify(p.returncode, output, timed_out)

    header = (f'# file: {path}\n'
              f'# suite: {suite}\n'
              f'# relpath: {rel}\n'
              f'# function: {fn}\n'
              f'# cmd: {" ".join(cmd)}\n'
              f'# rc: {p.returncode}\n'
              f'# status: {status}\n'
              f'# seconds: {secs:.2f}\n'
              f'# ------------------------------------------------------------\n')
    if timed_out:
        output += f'\n*** TIMEOUT after {args.timeout}s ***\n'
    log.parent.mkdir(parents=True, exist_ok=True)
    tmp = log.with_suffix('.log.tmp')
    tmp.write_text(header + output)
    tmp.rename(log)  # atomic, so a present .log is always complete

    return {'file': str(path), 'suite': suite, 'relpath': str(rel),
            'function': fn, 'index': idx,
            'log': str(log.relative_to(args.out)), 'rc': p.returncode,
            'status': status, 'detail': detail, 'seconds': round(secs, 2), **counts}


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('--llvm', type=Path,
                    default=Path.home() / 'llvm-project' / 'llvm',
                    help='LLVM source dir (containing test/)')
    ap.add_argument('--backend-tv', type=Path,
                    default=REPO / 'build' / 'backend-tv')
    ap.add_argument('--out', type=Path, required=True,
                    help='output directory for logs and manifest')
    ap.add_argument('--suites', default='transforms,riscv',
                    help='comma-separated subset of: transforms,riscv')
    ap.add_argument('--filter', default=None,
                    help='only files whose relpath matches this regex')
    ap.add_argument('-j', '--jobs', type=int, default=os.cpu_count())
    ap.add_argument('--timeout', type=float, default=60,
                    help='per-invocation wall-clock timeout in seconds')
    ap.add_argument('--mem-limit', type=int, default=8192,
                    help='per-invocation address-space limit in MB (0 = none)')
    ap.add_argument('--max-functions', type=int, default=None,
                    help='stop after this many invocations (for testing)')
    ap.add_argument('extra', nargs='*',
                    help='extra backend-tv arguments, after "--"')
    args = ap.parse_args()

    args.backend_tv = args.backend_tv.resolve()
    if not os.access(args.backend_tv, os.X_OK):
        sys.exit(f'backend-tv not found at {args.backend_tv}')
    args.out = args.out.resolve()
    (args.out / 'logs').mkdir(parents=True, exist_ok=True)
    # backend-tv runs here, so any stray files it writes stay out of the way
    args.workdir = args.out / 'work'
    args.workdir.mkdir(exist_ok=True)
    suites = set(args.suites.split(','))
    filt = re.compile(args.filter) if args.filter else None

    jobs = []
    skipped = 0
    nfiles = 0
    for suite, rel, path in collect_inputs(args.llvm, suites):
        if filt and not filt.search(str(rel)):
            continue
        nfiles += 1
        for idx, fn in enumerate(functions_in(path)):
            log = (args.out / 'logs' / suite / rel /
                   f'{idx:04d}-{safe_name(fn)}.log')
            if log.exists():
                skipped += 1
                continue
            jobs.append((suite, rel, path, idx, fn, log))
            if args.max_functions and len(jobs) >= args.max_functions:
                break
        if args.max_functions and len(jobs) >= args.max_functions:
            break
    print(f'{nfiles} files, {len(jobs)} invocations to run, '
          f'{skipped} already done', file=sys.stderr)

    manifest = open(args.out / 'manifest.jsonl', 'a')
    lock = threading.Lock()
    stats = Counter()
    t0 = time.monotonic()
    with ThreadPoolExecutor(max_workers=args.jobs) as ex:
        futs = [ex.submit(run_one, j, args) for j in jobs]
        for n, f in enumerate(as_completed(futs), 1):
            try:
                rec = f.result()
            except Exception as e:
                print(f'internal error: {e!r}', file=sys.stderr)
                stats['script-error'] += 1
                continue
            with lock:
                manifest.write(json.dumps(rec) + '\n')
                manifest.flush()
            stats[rec['status']] += 1
            if n % 200 == 0 or n == len(futs):
                el = time.monotonic() - t0
                print(f'\r[{n}/{len(futs)} {el:.0f}s] ' +
                      ' '.join(f'{k}={v}' for k, v in stats.most_common()),
                      end='', file=sys.stderr, flush=True)
    print(file=sys.stderr)
    manifest.close()

    # summary over the whole manifest, including earlier resumed runs
    total = Counter()
    with open(args.out / 'manifest.jsonl') as m:
        for line in m:
            total[json.loads(line)['status']] += 1
    with open(args.out / 'summary.txt', 'w') as s:
        for k, v in total.most_common():
            s.write(f'{k:20} {v}\n')
    print((args.out / 'summary.txt').read_text(), end='')


if __name__ == '__main__':
    main()
