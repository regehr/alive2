define ptr @f(ptr %0, ptr %1) {
  %3 = load <32 x i8>, ptr %0, align 4
  %4 = shufflevector <32 x i8> %3, <32 x i8> undef, <16 x i32> <i32 0, i32 2, i32 4, i32 6, i32 8, i32 10, i32 12, i32 14, i32 16, i32 18, i32 20, i32 22, i32 24, i32 26, i32 28, i32 30>
  %5 = shufflevector <32 x i8> %3, <32 x i8> undef, <16 x i32> <i32 1, i32 3, i32 5, i32 7, i32 9, i32 11, i32 13, i32 15, i32 17, i32 19, i32 21, i32 23, i32 25, i32 27, i32 29, i32 31>
  %6 = add <16 x i8> %4, %5
  store <16 x i8> %6, ptr %1, align 16
  %7 = getelementptr inbounds <32 x i8>, ptr %0, i32 1
  ret ptr %7
}