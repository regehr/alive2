define void @f(<8 x i8> %0, <8 x i8> %1, ptr nocapture writeonly %2) {
  %4 = zext <8 x i8> %0 to <8 x i16>
  %5 = zext <8 x i8> %1 to <8 x i16>
  %6 = add nuw nsw <8 x i16> %4, <i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1>
  %7 = add nuw nsw <8 x i16> %6, %5
  %8 = lshr <8 x i16> %7, <i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1>
  %9 = trunc <8 x i16> %8 to <8 x i8>
  store <8 x i8> %9, ptr %2, align 8
  ret void
}