define <8 x i8> @f(<8 x i16> %0, <8 x i8> %1) {
  %3 = ashr <8 x i16> %0, <i16 11, i16 11, i16 11, i16 11, i16 11, i16 11, i16 11, i16 11>
  %4 = sext <8 x i8> %1 to <8 x i16>
  %5 = add nsw <8 x i16> %3, %4
  %6 = ashr <8 x i16> %5, <i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1>
  %7 = trunc <8 x i16> %6 to <8 x i8>
  ret <8 x i8> %7
}