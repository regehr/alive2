define <8 x i16> @f(<8 x i16> %0, <8 x i8> %1) {
  %3 = sext <8 x i8> %1 to <8 x i16>
  %4 = sub <8 x i16> %0, %3
  ret <8 x i16> %4
}