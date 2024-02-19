define void @f(<16 x i8> %0, ptr nocapture writeonly %1) {
  %3 = zext <16 x i8> %0 to <16 x i16>
  %4 = add nuw nsw <16 x i16> %3, <i16 10, i16 10, i16 10, i16 10, i16 10, i16 10, i16 10, i16 10, i16 10, i16 10, i16 10, i16 10, i16 10, i16 10, i16 10, i16 10>
  %5 = lshr <16 x i16> %4, <i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1>
  %6 = trunc <16 x i16> %5 to <16 x i8>
  store <16 x i8> %6, ptr %1, align 16
  ret void
}