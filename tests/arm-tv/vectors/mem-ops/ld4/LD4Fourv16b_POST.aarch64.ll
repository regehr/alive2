define ptr @f(ptr %0, ptr %1) {
  %3 = load <64 x i8>, ptr %0, align 4
  %4 = shufflevector <64 x i8> %3, <64 x i8> undef, <16 x i32> <i32 0, i32 4, i32 8, i32 12, i32 16, i32 20, i32 24, i32 28, i32 32, i32 36, i32 40, i32 44, i32 48, i32 52, i32 56, i32 60>
  store <16 x i8> %4, ptr %1, align 16
  %5 = getelementptr inbounds <64 x i8>, ptr %0, i32 1
  ret ptr %5
}