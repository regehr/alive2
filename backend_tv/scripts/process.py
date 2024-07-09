#!/usr/bin/env python3
import sys
import pandas as pd
import numpy as np

csv_file = sys.argv[1]

def main():
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
  reg = regressed[num_cols].clip(0,1).sum().div(len(regressed))
  nonreg = nonregressed[num_cols].clip(0,1).sum().div(len(nonregressed))

  # print(reg.sort_values())
  # print(nonreg.sort_values())

  # identify encodings likely to cause timeout by 
  rel = pd.DataFrame({'regressed':reg, 'nonregressed':nonreg})
  rel = rel.loc[(rel['regressed'] != 0) | (rel['nonregressed'] != 0)]
  rel['relative'] = rel['regressed'] / rel['nonregressed']
  rel.sort_values('relative', inplace=True)
  print(rel.tail(10).to_string())

  # TODO: we should look at encodings which are in /no/ successful tests.

if __name__ == '__main__':
  main()
