; Function Attrs: nounwind
define void @f(ptr %0, ptr %1) {
  %3 = load <64 x i8>, ptr %0, align 64
  %4 = shufflevector <64 x i8> %3, <64 x i8> undef, <16 x i32> <i32 2, i32 6, i32 10, i32 14, i32 18, i32 22, i32 26, i32 30, i32 34, i32 38, i32 42, i32 46, i32 50, i32 54, i32 58, i32 62>
  store <16 x i8> %4, ptr %1, align 16
  ret void
}