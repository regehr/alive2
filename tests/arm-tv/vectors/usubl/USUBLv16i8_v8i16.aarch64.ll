define <16 x i8> @f(<16 x i8> %0, <16 x i8> %1) {
  %3 = zext <16 x i8> %0 to <16 x i16>
  %4 = zext <16 x i8> %1 to <16 x i16>
  %5 = sub <16 x i16> %3, %4
  %6 = trunc <16 x i16> %5 to <16 x i8>
  ret <16 x i8> %6
}