; Function Attrs: nounwind
define <8 x i16> @f(<8 x i16> %0, <8 x i8> %1, <8 x i8> %2) {
  %4 = zext <8 x i8> %1 to <8 x i16>
  %5 = zext <8 x i8> %2 to <8 x i16>
  %6 = mul <8 x i16> %4, %5
  %7 = add <8 x i16> %0, %6
  %8 = and <8 x i16> %7, <i16 255, i16 255, i16 255, i16 255, i16 255, i16 255, i16 255, i16 255>
  ret <8 x i16> %8
}