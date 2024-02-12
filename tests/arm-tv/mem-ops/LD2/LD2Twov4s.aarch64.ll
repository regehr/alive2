; Function Attrs: nounwind
define void @f(ptr %0, ptr %1) {
  %3 = load <8 x i32>, ptr %0, align 32
  %4 = shufflevector <8 x i32> %3, <8 x i32> undef, <4 x i32> <i32 0, i32 2, i32 4, i32 6>
  store <4 x i32> %4, ptr %1, align 16
  ret void
}