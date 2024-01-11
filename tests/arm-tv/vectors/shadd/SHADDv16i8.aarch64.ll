define void @f(<16 x i8> %0, <16 x i8> %1, ptr nocapture writeonly %2) {
  %4 = sext <16 x i8> %0 to <16 x i16>
  %5 = sext <16 x i8> %1 to <16 x i16>
  %6 = add nsw <16 x i16> %4, %5
  %7 = lshr <16 x i16> %6, <i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1>
  %8 = trunc <16 x i16> %7 to <16 x i8>
  store <16 x i8> %8, ptr %2, align 16
  ret void
}