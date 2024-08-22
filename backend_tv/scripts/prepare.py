#!/usr/bin/env python3

import io
import os
import csv
import sys
import random
import tarfile
import functools
import subprocess
import dataclasses
import concurrent.futures
from subprocess import PIPE, DEVNULL
from collections import defaultdict
from pathlib import Path
from typing import cast, Any

try:
  from tqdm import tqdm # type: ignore
except ImportError:
  def tqdm(it, /, total=None, mininterval=None):
    if total is None: total = len(it)
    ndone = 0
    i = 0
    split = total // 256
    for x in it:
      ndone += 1
      i += 1
      if ndone == 1 or i >= split:
        i = 0
        print(f'\r{ndone/total:.2%}', end=' ', flush=True, file=sys.stderr)
      yield x


root_dir = Path(sys.argv[1] if len(sys.argv) >= 2 else '/nowhere')
logs_dir = root_dir / 'logs'
aslplogs_dir = root_dir / 'logs-aslp'
assert logs_dir.is_dir() and aslplogs_dir.is_dir(), ( # XXX: changed from previous command-line of `prepare.py logs logs-aslp`
  f"first argument should be a directory containing 'logs' and 'logs-aslp'")

out_csv = Path(sys.argv[2] if len(sys.argv) >= 3 else 'table.tar.gz')

self_dir = Path(os.path.dirname(os.path.realpath(__file__)))
classify_pl = self_dir / 'classify.pl'

@dataclasses.dataclass
class Row:
  id: str
  old_outcome: str
  old_detail: str
  aslp_outcome: str
  aslp_detail: str
  encoding_counts: dict[str, int]
  cmdline: str
  assembly: str
  old_output: str
  aslp_output: str

def extract_asm_and_output(log):
  with open(log) as f:
    d = f.read().split('\n------------')

  asm = output = ''
  match d:
    case _header, asm, _debug, output, *_:
      assert 'AArch64 Assembly' in asm, log
      # assert '\n=>\n' in output, log
      asm = '\n'.join(asm.splitlines()[1:])
      output = output.lstrip('-')
  return asm, output

@functools.lru_cache
def classify_logs(d: Path) -> dict[Path, str]:
  proc = subprocess.run([classify_pl, d], stdin=DEVNULL, stdout=PIPE, encoding='utf-8')
  out = proc.stdout.strip()
  # assert not err, f"classify.py {d} returned error: {err}"
  return {Path(p): v for p,v in map(lambda l: l.split('|')[:2], out.splitlines())}


def process_log(log: Path, aslplog: Path) -> Row:
  out = classify_logs(logs_dir)[log]
  aout = classify_logs(aslplogs_dir)[aslplog]

  def simplify_name(s):
    if s.startswith('classic_'):
      s = s.replace('classic_', '', 1)
      out = ''
      for c in s:
        if c == c.lower():
          break
        out += c
      return 'classic_' + out
    return s

  # process encoding counts
  counts = defaultdict(int)
  cmdline = ''
  with open(aslplog) as f:
    for l in f:
      l = l.strip()
      if not cmdline and l: cmdline = l
      if not l.startswith('encoding counts: '): continue
      for term in l.replace('encoding counts: ', '').split(','):
        if '=' not in term: continue
        k,v = term.split('=')
        counts[simplify_name(k)] += int(v)
      break
  assert cmdline, aslplog


  asm, output = extract_asm_and_output(log)
  _, aoutput = extract_asm_and_output(aslplog)

  def outcome(s):
    assert s.startswith('['), f'{log}: >{s!r}<'
    return s.split(' ', 1)[0]

  return Row(log.stem, outcome(out), out, outcome(aout), aout, counts, cmdline, asm, output, aoutput)

def main():
  logs = list(logs_dir.glob('*.log'))

  classified: dict[Path, Row] = cast(Any, {f: () for f in logs})
  random.shuffle(logs) # XXX: be aware of shuffle!

  aslplogs = [aslplogs_dir / f.name for f in logs]
  for f in aslplogs:
    assert f.exists(), f

  with concurrent.futures.ThreadPoolExecutor(2) as executor:
    _, _ = executor.map(classify_logs, [logs_dir, aslplogs_dir])

  with concurrent.futures.ProcessPoolExecutor(4) as executor:
    futures = executor.map(process_log, logs, aslplogs)
    for f, result in tqdm(zip(logs, futures), total=len(logs), mininterval=1):
      classified[f] = result

  keys = ['id']
  keys += sorted(set(k for x in classified.values() for k in x.encoding_counts.keys()))
  keys += ['old_outcome']
  keys += ['old_detail']
  keys += ['aslp_outcome']
  keys += ['aslp_detail']
  keys += ['cmdline']
  keys += ['old_output']
  keys += ['aslp_output']

  csvfile = io.StringIO()
  writer = csv.DictWriter(csvfile, fieldnames=keys, restval=0)
  writer.writeheader()

  for f, row in tqdm(classified.items()):
    r = {'id': f.stem, 'old_outcome': row.old_outcome, 'old_detail': row.old_detail,
           'aslp_outcome': row.aslp_outcome, 'aslp_detail': row.aslp_detail, 'cmdline': row.cmdline,
           'old_output': row.old_output, 'aslp_output': row.aslp_output}
    r |= row.encoding_counts
    assert set(r.keys()) <= set(keys)
    writer.writerow(r) # type: ignore

  data = csvfile.getvalue().encode('utf-8')
  with tarfile.open(out_csv, 'w:gz') as t:
    ti = tarfile.TarInfo('table.csv')
    ti.size = len(data)
    t.addfile(ti, io.BytesIO(data))
  print(out_csv)

if __name__ == '__main__':
  main()
