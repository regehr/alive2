define void @f(ptr %0, ptr %1) {
  %3 = load <4 x i32>, ptr %0, align 4
  %4 = shufflevector <4 x i32> %3, <4 x i32> poison, <2 x i32> <i32 1, i32 3>
  store <2 x i32> %4, ptr %1, align 4
  ret void
}