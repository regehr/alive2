; Function Attrs: nounwind
define void @f(ptr %0, ptr %1, ptr %2, ptr %3) {
  %5 = load <24 x i32>, ptr %0, align 4
  %6 = shufflevector <24 x i32> %5, <24 x i32> undef, <8 x i32> <i32 0, i32 3, i32 6, i32 9, i32 12, i32 15, i32 18, i32 21>
  store <8 x i32> %6, ptr %1, align 4
  ret void
}