#!/usr/bin/env python3
import sys
import pandas as pd
import numpy as np

csv_file = sys.argv[1]

def main():
  df = pd.read_csv(csv_file, index_col=0)

  # drop instruction name for unsupported instructions to combine rows
  df["old_detail"] = df["old_detail"].apply(
    lambda x: x if 'Unsupported AArch64 instruction:' not in x else ':'.join(x.split(':')[:2])
  )

  # rows with changed outcomes
  changed = df.loc[~(df['old_outcome'] == df['aslp_outcome'])]

  # tabulates changes
  print(changed[['old_detail', 'aslp_detail']].value_counts().to_string())

  print()
  print()
  # print(df.loc[df.old_detail.str.startswith('[i] ERROR: Unsu')])

  # total counts of each category
  print()
  print(df[['old_outcome']].value_counts())
  print()
  print(df[['aslp_outcome']].value_counts())

  regressed = df.loc[(df['old_outcome'] == '[c]') & (df['aslp_outcome'] != '[c]')]
  num_cols = df.select_dtypes(np.number).columns
  # for each encoding, computes the proportion of regressed tests in which it appears
  print(regressed[num_cols].clip(0,1).sum().div(len(regressed)).sort_values().to_string())

if __name__ == '__main__':
  main()
