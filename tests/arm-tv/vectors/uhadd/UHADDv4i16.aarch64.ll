define void @f(<4 x i16> %0, ptr nocapture writeonly %1) {
  %3 = zext <4 x i16> %0 to <4 x i32>
  %4 = add nuw nsw <4 x i32> %3, <i32 10, i32 10, i32 10, i32 10>
  %5 = lshr <4 x i32> %4, <i32 1, i32 1, i32 1, i32 1>
  %6 = trunc <4 x i32> %5 to <4 x i16>
  store <4 x i16> %6, ptr %1, align 8
  ret void
}