define void @f(ptr %0, ptr %1) {
  %3 = load <8 x i16>, ptr %0, align 2
  %4 = shufflevector <8 x i16> %3, <8 x i16> poison, <4 x i32> <i32 0, i32 2, i32 4, i32 6>
  store <4 x i16> %4, ptr %1, align 2
  ret void
}