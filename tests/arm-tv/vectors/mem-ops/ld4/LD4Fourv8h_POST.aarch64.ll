; Function Attrs: nounwind
define void @f(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4) {
  %6 = load <64 x i16>, ptr %0, align 64
  %7 = shufflevector <64 x i16> %6, <64 x i16> poison, <16 x i32> <i32 0, i32 4, i32 8, i32 12, i32 16, i32 20, i32 24, i32 28, i32 32, i32 36, i32 40, i32 44, i32 48, i32 52, i32 56, i32 60>
  store <16 x i16> %7, ptr %1, align 64
  ret void
}