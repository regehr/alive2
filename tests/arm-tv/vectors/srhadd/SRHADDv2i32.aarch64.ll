define void @f(<2 x i32> %0, <2 x i32> %1, ptr nocapture writeonly %2) {
  %4 = sext <2 x i32> %0 to <2 x i64>
  %5 = sext <2 x i32> %1 to <2 x i64>
  %6 = add nsw <2 x i64> %4, <i64 1, i64 1>
  %7 = add nsw <2 x i64> %6, %5
  %8 = lshr <2 x i64> %7, <i64 1, i64 1>
  %9 = trunc <2 x i64> %8 to <2 x i32>
  store <2 x i32> %9, ptr %2, align 8
  ret void
}