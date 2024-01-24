define <2 x i64> @f(<4 x i32> %0, <4 x i32> %1) {
  %3 = shufflevector <4 x i32> %0, <4 x i32> undef, <2 x i32> <i32 2, i32 3>
  %4 = zext <2 x i32> %3 to <2 x i64>
  %5 = shufflevector <4 x i32> %1, <4 x i32> undef, <2 x i32> <i32 2, i32 3>
  %6 = zext <2 x i32> %5 to <2 x i64>
  %7 = sub <2 x i64> %4, %6
  ret <2 x i64> %7
}