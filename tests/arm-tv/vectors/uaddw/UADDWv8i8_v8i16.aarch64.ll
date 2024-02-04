define <8 x i16> @f(<8 x i16> %0, <8 x i8> %1) {
  %3 = zext <8 x i8> %1 to <8 x i16>
  %4 = add <8 x i16> %0, %3
  ret <8 x i16> %4
}