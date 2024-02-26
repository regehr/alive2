define void @f(<1 x i64> %0, ptr %1) {
  %3 = add <1 x i64> %0, <i64 16>
  %4 = lshr <1 x i64> %3, <i64 5>
  %5 = trunc <1 x i64> %4 to <1 x i32>
  store <1 x i32> %5, ptr %1, align 4
  ret void
}