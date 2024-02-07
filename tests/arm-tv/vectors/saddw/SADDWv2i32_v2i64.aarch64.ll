define <2 x i64> @f(<2 x i64> %0, <2 x i32> %1) {
  %3 = sext <2 x i32> %1 to <2 x i64>
  %4 = add <2 x i64> %3, %0
  ret <2 x i64> %4
}