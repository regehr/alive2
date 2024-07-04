#!/usr/bin/env python3
import sys
import pandas as pd

csv_file = sys.argv[1]

def main():
  df = pd.read_csv(csv_file, index_col=0)

  changed = df.loc[~(df['old_outcome'] == df['aslp_outcome'])]
  print(changed[['old_detail', 'aslp_detail']].value_counts())

  print(df.loc[df.old_detail.str.startswith('[i] ERROR: Unsu')])

if __name__ == '__main__':
  main()
