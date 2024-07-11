; Function Attrs: nounwind
define <1 x i64> @f(<1 x i64> %0) {
  %2 = ashr <1 x i64> %0, <i64 42>
  ret <1 x i64> %2
}