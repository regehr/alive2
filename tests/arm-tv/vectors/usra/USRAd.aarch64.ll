define <1 x i64> @f(<2 x i32> %0) {
  %2 = lshr <2 x i32> %0, <i32 31, i32 31>
  %3 = bitcast <2 x i32> %2 to <1 x i64>
  %4 = lshr <1 x i64> %3, <i64 31>
  %5 = or <1 x i64> %4, %3
  ret <1 x i64> %5
}