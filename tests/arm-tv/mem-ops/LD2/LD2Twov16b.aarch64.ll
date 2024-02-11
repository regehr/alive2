; Function Attrs: nounwind
define void @f(ptr %0, ptr %1) {
  %3 = load <32 x i8>, ptr %0, align 32
  %4 = shufflevector <32 x i8> %3, <32 x i8> undef, <16 x i32> <i32 0, i32 2, i32 4, i32 6, i32 8, i32 10, i32 12, i32 14, i32 16, i32 18, i32 20, i32 22, i32 24, i32 26, i32 28, i32 30>
  store <16 x i8> %4, ptr %1, align 16
  ret void
}