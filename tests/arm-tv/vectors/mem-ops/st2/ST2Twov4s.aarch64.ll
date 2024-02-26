define void @f(ptr %0, <4 x i32> %1, <4 x i32> %2) {
  %4 = shufflevector <4 x i32> %1, <4 x i32> %2, <8 x i32> <i32 poison, i32 poison, i32 poison, i32 poison, i32 2, i32 6, i32 3, i32 7>
  store <8 x i32> %4, ptr %0, align 4
  ret void
}