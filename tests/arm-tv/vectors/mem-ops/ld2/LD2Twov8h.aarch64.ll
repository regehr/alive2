; Function Attrs: nounwind
define void @f(ptr %0, ptr %1) {
  %3 = load <16 x i16>, ptr %0, align 32
  %4 = shufflevector <16 x i16> %3, <16 x i16> undef, <8 x i32> <i32 0, i32 2, i32 4, i32 6, i32 8, i32 10, i32 12, i32 14>
  store <8 x i16> %4, ptr %1, align 16
  ret void
}