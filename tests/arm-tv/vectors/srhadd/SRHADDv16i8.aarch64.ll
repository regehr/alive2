define void @f(ptr %0, ptr %1, ptr %2) {
  %4 = load <16 x i8>, ptr %0, align 16
  %5 = load <16 x i8>, ptr %1, align 16
  %6 = sext <16 x i8> %4 to <16 x i16>
  %7 = sext <16 x i8> %5 to <16 x i16>
  %8 = add nuw nsw <16 x i16> %6, <i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1>
  %9 = add nuw nsw <16 x i16> %8, %7
  %10 = lshr <16 x i16> %9, <i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1>
  %11 = trunc <16 x i16> %10 to <16 x i8>
  store <16 x i8> %11, ptr %0, align 16
  ret void
}