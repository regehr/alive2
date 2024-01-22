define <8 x i16> @f(<8 x i16> %0, <8 x i8> %1, <8 x i8> %2, <8 x i8> %3, <8 x i8> %4) {
  %6 = sext <8 x i8> %1 to <8 x i16>
  %7 = sext <8 x i8> %2 to <8 x i16>
  %8 = sext <8 x i8> %3 to <8 x i16>
  %9 = sext <8 x i8> %4 to <8 x i16>
  %10 = mul nsw <8 x i16> %7, %6
  %11 = mul nsw <8 x i16> %9, %8
  %12 = add <8 x i16> %11, %10
  %13 = sub <8 x i16> %0, %12
  ret <8 x i16> %13
}