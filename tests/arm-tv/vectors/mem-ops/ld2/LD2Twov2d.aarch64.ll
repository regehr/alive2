define void @f(ptr %0, ptr %1) {
  %3 = load <4 x i64>, ptr %0, align 8
  %4 = shufflevector <4 x i64> %3, <4 x i64> poison, <2 x i32> <i32 0, i32 2>
  store <2 x i64> %4, ptr %1, align 8
  ret void
}