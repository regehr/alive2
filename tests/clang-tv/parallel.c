// TEST-ARGS: -O3 -mllvm -tv-tgt-unroll=2 -mllvm -tv-src-unroll=2 -mllvm -tv-smt-to=10000 -mllvm -tv-report-dir=. -mllvm -tv-overwrite-report

#include <math.h>
#include <stdio.h>

int repeat(char *num, int n) {
  int count[10];
  for (int i = 0; i <= 9; ++i)
    count[i] = 0;
  for (int i = 0; i < n; ++i) {
    if (count[num[i]] > 0)
      return 1;
    count[num[i]]++;
  }
  return 0;
}

int next(char *num, int n) {
  for (int i = 0; i < n; ++i) {
    if (num[i] != 9) {
      num[i]++;
      return 0;
    } else {
      num[i] = 0;
    }
  }
  return 1;
}

void check(const int top, const int bottom) {
  const int n = top + bottom;
  char num[n + 1];
  for (int i = 0; i < 1 + n; ++i)
    num[i] = 0;
  double max = 0.0;
  while (1) {
    if (!repeat(num, n) && num[0] != 0 && num[top] != 0) {
      int mul = 1;
      int top_n = 0, bottom_n = 0;
      for (int i = top - 1; i >= 0; --i) {
        top_n += mul * num[i];
        mul *= 10;
      }
      mul = 1;
      for (int i = bottom - 1; i >= 0; --i) {
        bottom_n += mul * num[i + top];
        mul *= 10;
      }
      double frac = (double)top_n / (double)bottom_n;
      if (frac < 1.0 && frac > max) {
        printf("%d %d %lf\n", top_n, bottom_n, frac);
        max = frac;
      }
    }
    if (next(num, n))
      return;
  }
}

int main(void) {
  check(5, 5);
  return 0;
}
