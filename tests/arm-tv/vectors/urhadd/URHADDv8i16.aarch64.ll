define void @f(<8 x i16> %0, <8 x i16> %1, ptr nocapture writeonly %2) {
  %4 = zext <8 x i16> %0 to <8 x i32>
  %5 = zext <8 x i16> %1 to <8 x i32>
  %6 = add nuw nsw <8 x i32> %4, <i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1>
  %7 = add nuw nsw <8 x i32> %6, %5
  %8 = lshr <8 x i32> %7, <i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1>
  %9 = trunc <8 x i32> %8 to <8 x i16>
  store <8 x i16> %9, ptr %2, align 16
  ret void
}