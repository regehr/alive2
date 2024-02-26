define void @f(<4 x i32> %0, ptr nocapture writeonly %1) {
  %3 = zext <4 x i32> %0 to <4 x i64>
  %4 = add nuw nsw <4 x i64> %3, <i64 10, i64 10, i64 10, i64 10>
  %5 = lshr <4 x i64> %4, <i64 1, i64 1, i64 1, i64 1>
  %6 = trunc <4 x i64> %5 to <4 x i32>
  store <4 x i32> %6, ptr %1, align 16
  ret void
}