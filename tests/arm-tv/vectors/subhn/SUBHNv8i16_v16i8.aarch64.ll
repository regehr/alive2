define <16 x i8> @f(<8 x i8> %0, <8 x i16> %1, <8 x i16> %2) {
  %4 = sub <8 x i16> %1, %2
  %5 = lshr <8 x i16> %4, <i16 8, i16 8, i16 8, i16 8, i16 8, i16 8, i16 8, i16 8>
  %6 = trunc <8 x i16> %5 to <8 x i8>
  %7 = bitcast <8 x i8> %0 to <1 x i64>
  %8 = bitcast <8 x i8> %6 to <1 x i64>
  %9 = shufflevector <1 x i64> %7, <1 x i64> %8, <2 x i32> <i32 0, i32 1>
  %10 = bitcast <2 x i64> %9 to <16 x i8>
  ret <16 x i8> %10
}