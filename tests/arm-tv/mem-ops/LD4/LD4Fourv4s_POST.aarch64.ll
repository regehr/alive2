define ptr @f(ptr %0, ptr %1) {
  %3 = load <16 x i32>, ptr %0, align 4
  %4 = shufflevector <16 x i32> %3, <16 x i32> undef, <4 x i32> <i32 0, i32 4, i32 8, i32 12>
  store <4 x i32> %4, ptr %1, align 16
  %5 = getelementptr inbounds <16 x i32>, ptr %0, i32 1
  ret ptr %5
}