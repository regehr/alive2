define <2 x i64> @f(<2 x i64> %0, <4 x i32> %1) {
  %3 = shufflevector <4 x i32> %1, <4 x i32> undef, <2 x i32> <i32 2, i32 3>
  %4 = zext <2 x i32> %3 to <2 x i64>
  %5 = sub <2 x i64> %0, %4
  ret <2 x i64> %5
}