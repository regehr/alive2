#!/usr/bin/env python3
# vim: sw=2 ts=2 et sts=2

import os
import sys
import shutil
from pathlib import Path

import pandas as pd
import numpy as np

csv_file = sys.argv[1]

def underline(s):
  print()
  print(s)
  print('-'*len(s))
  print(flush=True)

def make_retry_folder(df, name):
  d = Path('.') / ('retry-' + name)
  d2 = Path('.') / ('retry-logs-' + name)
  print(f'making retry folder {d} for {len(df)} tests')
  
  if d.exists() and d.is_dir(): shutil.rmtree(d)
  os.makedirs(d, exist_ok=False)

  if d2.exists() and d2.is_dir(): shutil.rmtree(d2)
  os.makedirs(d2, exist_ok=False)

  for i, _ in df.iterrows():
    shutil.copy((Path.home() / 'Downloads/arm-tests' / i.replace('_0','')).with_suffix('.bc'), d)

  for i, _ in df.iterrows():
    i = Path(i).with_suffix('.log')
    shutil.copy('logs' / i, d2 / i.with_suffix('.classic.log'))
    shutil.copy('logs-aslp' / i, d2 / i.with_suffix('.aslp.log'))

def timeout_regressions():
  underline('regressions')

  df = pd.read_csv(csv_file, index_col=0)

  # drop instruction name for unsupported instructions to reduce number of distinct rows
  df["old_detail"] = df["old_detail"].apply(
    lambda x: x if 'Unsupported AArch64 instruction:' not in x else ':'.join(x.split(':')[:2])
  )

  # rows with changed outcomes
  changed = df.loc[~(df['old_outcome'] == df['aslp_outcome'])]

  # tabulates changes
  print(changed[['old_detail', 'aslp_detail']].value_counts().to_string())
  print()
  print(changed[['old_outcome', 'aslp_outcome']].value_counts().to_string())

  print()
  print()
  # print(df.loc[df.old_detail.str.startswith('[i] ERROR: Unsu')])

  # total counts of each [c] [u] [i] [f] category
  print()
  print(df[['old_outcome']].value_counts())
  print()
  print(df[['aslp_outcome']].value_counts())

  # XXX: assume numeric columns are encoding counts
  num_cols = df.select_dtypes(np.number).columns

  regressed = df.loc[(df['old_outcome'] == '[c]') & (df['aslp_outcome'] != '[c]')]
  nonregressed = df.loc[(df['old_outcome'] == '[c]') & (df['aslp_outcome'] == '[c]')]

  # for each encoding, computes the proportion of (non)regressed tests in which it appears
  reg = regressed[num_cols].clip(0,1).sum()
  nonreg = nonregressed[num_cols].clip(0,1).sum()

  # print(reg.sort_values())
  # print(nonreg.sort_values())

  # identify encodings likely to cause timeout by 
  rel = pd.DataFrame({'regressed':reg, 'nonregressed':nonreg})
  rel = rel.loc[(rel['regressed'] != 0) | (rel['nonregressed'] != 0)]
  rel['relative'] = rel['regressed'] / rel['nonregressed']
  rel.sort_values('relative', inplace=True)
  underline('regressions by instruction')
  print(rel.tail(15).to_string())
  print()

  worst = rel.iloc[-1].name
  print(regressed.sort_values(worst).tail(10)[[worst, 'old_outcome', 'aslp_detail']].to_string())

  # TODO: we should look at encodings which are in /no/ successful tests.


  # underline('closer manual examination')
  more_poison = df.loc[(df['old_outcome'] == '[c]') & (df['aslp_detail'] == '[f] ERROR: Target is more poisonous than source')]
  # print(more_poison[['old_outcome', 'aslp_detail']])
  make_retry_folder(more_poison, 'more-poisonous')

  less_defined = df.loc[(df['old_outcome'] == '[c]') & (df['aslp_detail'] == '[f] ERROR: Source is more defined than target')]
  print(less_defined[['old_outcome', 'aslp_detail']])
  make_retry_folder(less_defined, 'less_defined')

  lexprvar = df.loc[(df['old_outcome'] == '[c]') & (df['aslp_detail'].str.contains('lexprvar unsup'))]
  print(lexprvar[['old_outcome', 'aslp_detail']])
  make_retry_folder(lexprvar, 'lexprvar')


  oldtimeouts = df.loc[(df['old_detail'] == '[u] Timeout') & (df['aslp_outcome'] == '[c]')]
  print(oldtimeouts[['old_outcome', 'aslp_detail']])
  make_retry_folder(oldtimeouts, 'old-timeouts')





def missing_instructions_progress():
  df = pd.read_csv(csv_file, index_col=0)

  # fetch only tests which failed due to unsupported instructions
  changed = (df.loc[df['old_outcome'] == '[i]'])

  # select only one test case for each failing instruction,
  # reducing the outsized impact of unsup instructions which reoccur often.
  # changed = changed.drop_duplicates(subset=['old_detail'])


  print(changed[['old_detail', 'aslp_detail']].value_counts().to_string())
  print()
  print(changed[['old_outcome', 'aslp_outcome']].value_counts().to_string())
  print(changed[['old_outcome', 'aslp_outcome']].value_counts(normalize=True).to_string())


if __name__ == '__main__':
  # missing_instructions_progress()
  timeout_regressions()
