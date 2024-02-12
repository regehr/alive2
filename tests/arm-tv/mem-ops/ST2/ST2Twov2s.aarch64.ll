; Function Attrs: nounwind
define void @f(ptr %0, ptr %1, ptr %2) {
  %4 = load <4 x i32>, ptr %1, align 16
  %5 = load <4 x i32>, ptr %2, align 16
  %6 = shufflevector <4 x i32> %4, <4 x i32> %5, <4 x i32> <i32 0, i32 0, i32 1, i32 1>
  store <4 x i32> %6, ptr %0, align 16
  ret void
}