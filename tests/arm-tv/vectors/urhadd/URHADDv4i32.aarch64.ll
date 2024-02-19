define void @f(<4 x i32> %0, <4 x i32> %1, ptr nocapture writeonly %2) {
  %4 = zext <4 x i32> %0 to <4 x i64>
  %5 = zext <4 x i32> %1 to <4 x i64>
  %6 = add nuw nsw <4 x i64> %4, <i64 1, i64 1, i64 1, i64 1>
  %7 = add nuw nsw <4 x i64> %6, %5
  %8 = lshr <4 x i64> %7, <i64 1, i64 1, i64 1, i64 1>
  %9 = trunc <4 x i64> %8 to <4 x i32>
  store <4 x i32> %9, ptr %2, align 16
  ret void
}