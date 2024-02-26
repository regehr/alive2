define void @f(<4 x i16> %0, <4 x i16> %1, ptr nocapture writeonly %2) {
  %4 = sext <4 x i16> %0 to <4 x i32>
  %5 = sext <4 x i16> %1 to <4 x i32>
  %6 = add nsw <4 x i32> %4, <i32 1, i32 1, i32 1, i32 1>
  %7 = add nsw <4 x i32> %6, %5
  %8 = lshr <4 x i32> %7, <i32 1, i32 1, i32 1, i32 1>
  %9 = trunc <4 x i32> %8 to <4 x i16>
  store <4 x i16> %9, ptr %2, align 8
  ret void
}