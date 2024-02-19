; Function Attrs: nounwind
define <16 x i8> @f(<16 x i8> %0, <16 x i8> %1) {
  %3 = zext <16 x i8> %0 to <16 x i16>
  %4 = zext <16 x i8> %1 to <16 x i16>
  %5 = add nuw nsw <16 x i16> %3, %4
  %6 = add nuw nsw <16 x i16> %5, <i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1>
  %7 = lshr <16 x i16> %6, <i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1>
  %8 = trunc <16 x i16> %7 to <16 x i8>
  ret <16 x i8> %8
}