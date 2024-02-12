; Function Attrs: nounwind
define void @f(ptr %0, ptr %1, ptr %2, ptr %3) {
  %5 = load <12 x i64>, ptr %0, align 64
  %6 = shufflevector <12 x i64> %5, <12 x i64> poison, <4 x i32> <i32 0, i32 3, i32 6, i32 9>
  store <4 x i64> %6, ptr %1, align 64
  ret void
}