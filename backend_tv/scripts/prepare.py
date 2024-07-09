#!/usr/bin/env python3

import io
import os
import csv
import sys
import tarfile
import random
import subprocess
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


logs_dir = Path(sys.argv[1])
aslplogs_dir = Path(sys.argv[2])
assert logs_dir.is_dir()
assert aslplogs_dir.is_dir()


self_dir = Path(os.path.dirname(os.path.realpath(__file__)))
classify_pl = self_dir / 'classify.pl'


def process_log(log: Path, aslplog: Path):
  proc = subprocess.run([classify_pl, log], stdin=DEVNULL, stdout=PIPE, stderr=PIPE, encoding='utf-8')
  out, err = map(str.strip, (proc.stdout, proc.stderr))
  assert not err, f"{log} returned error: {err}"

  proc = subprocess.run([classify_pl, aslplog], stdin=DEVNULL, stdout=PIPE, stderr=PIPE, encoding='utf-8')
  aout, aerr = map(str.strip, (proc.stdout, proc.stderr))
  assert not err, f"{aslplog} returned error: {aerr}"

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

  counts = defaultdict(int)
  with open(aslplog) as f:
    for l in f:
      if not l.startswith('encoding counts: '): continue
      for term in l.replace('encoding counts: ', '').split(','):
        if '=' not in term: continue
        k,v = term.split('=')
        counts[simplify_name(k)] += int(v)
      break

  return out, aout, counts

def main():
  logs = list(logs_dir.glob('*.log'))
  for f in logs:
    assert (aslplogs_dir / f.name).exists()

  classified: dict[Path, tuple[str, str, dict]] = cast(Any, {f: () for f in logs})
  random.shuffle(logs)

  with concurrent.futures.ThreadPoolExecutor() as pool:
    futures = {pool.submit(process_log, f, aslplogs_dir / f.name): f for f in logs}
    for f in tqdm(concurrent.futures.as_completed(futures.keys()), total=len(futures), mininterval=1):
      classified[futures[f]] = f.result()


  keys = ['id']
  keys += sorted(set(k for _,_,x in classified.values() for k in x.keys()))
  keys += ['old_outcome']
  keys += ['old_detail']
  keys += ['aslp_outcome']
  keys += ['aslp_detail']

  csvfile = io.StringIO()
  writer = csv.DictWriter(csvfile, fieldnames=keys, restval=0)
  writer.writeheader()

  for f, (classic, aslp, counts) in tqdm(classified.items()):
    t = lambda x: x.split(' ')[0]
    row = {'id': f.stem, 'old_outcome': t(classic), 'old_detail': classic, 'aslp_outcome': t(aslp), 'aslp_detail': aslp}
    row |= counts
    assert set(row.keys()) <= set(keys)
    writer.writerow(row) # type: ignore

  data = csvfile.getvalue().encode('utf-8')
  with tarfile.open('table.tar.gz', 'w:gz') as t:
    ti = tarfile.TarInfo('table.csv')
    ti.size = len(data)
    t.addfile(ti, io.BytesIO(data))

if __name__ == '__main__':
  main()
