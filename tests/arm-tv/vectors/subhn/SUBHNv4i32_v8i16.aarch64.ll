define <8 x i16> @f(<4 x i16> %0, <4 x i32> %1, <4 x i32> %2) {
  %4 = sub <4 x i32> %1, %2
  %5 = lshr <4 x i32> %4, <i32 16, i32 16, i32 16, i32 16>
  %6 = trunc <4 x i32> %5 to <4 x i16>
  %7 = bitcast <4 x i16> %0 to <1 x i64>
  %8 = bitcast <4 x i16> %6 to <1 x i64>
  %9 = shufflevector <1 x i64> %7, <1 x i64> %8, <2 x i32> <i32 0, i32 1>
  %10 = bitcast <2 x i64> %9 to <8 x i16>
  ret <8 x i16> %10
}