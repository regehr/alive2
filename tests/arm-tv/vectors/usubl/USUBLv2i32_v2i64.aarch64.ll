define <2 x i64> @f(<2 x i32> %0, <2 x i32> %1) {
  %3 = zext <2 x i32> %0 to <2 x i64>
  %4 = zext <2 x i32> %1 to <2 x i64>
  %5 = sub <2 x i64> %3, %4
  ret <2 x i64> %5
}