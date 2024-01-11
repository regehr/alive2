define void @f(<2 x i32> %0, ptr nocapture writeonly %1) {
  %3 = sext <2 x i32> %0 to <2 x i64>
  %4 = add nsw <2 x i64> %3, <i64 10, i64 10>
  %5 = lshr <2 x i64> %4, <i64 1, i64 1>
  %6 = trunc <2 x i64> %5 to <2 x i32>
  store <2 x i32> %6, ptr %1, align 8
  ret void
}