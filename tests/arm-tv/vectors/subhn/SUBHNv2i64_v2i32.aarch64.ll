define <2 x i32> @f(<2 x i64> %0, <2 x i64> %1) {
  %3 = sub <2 x i64> %0, %1
  %4 = lshr <2 x i64> %3, <i64 32, i64 32>
  %5 = trunc <2 x i64> %4 to <2 x i32>
  ret <2 x i32> %5
}