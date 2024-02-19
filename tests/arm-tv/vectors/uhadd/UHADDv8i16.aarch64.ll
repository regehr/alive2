define void @f(<8 x i16> %0, ptr nocapture writeonly %1) {
  %3 = zext <8 x i16> %0 to <8 x i32>
  %4 = add nuw nsw <8 x i32> %3, <i32 10, i32 10, i32 10, i32 10, i32 10, i32 10, i32 10, i32 10>
  %5 = lshr <8 x i32> %4, <i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1>
  %6 = trunc <8 x i32> %5 to <8 x i16>
  store <8 x i16> %6, ptr %1, align 16
  ret void
}