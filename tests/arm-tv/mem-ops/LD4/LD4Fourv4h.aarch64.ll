; Function Attrs: nounwind
define void @f(ptr %0, ptr %1) {
  %3 = load <16 x i16>, ptr %0, align 32
  %4 = shufflevector <16 x i16> %3, <16 x i16> undef, <4 x i32> <i32 1, i32 5, i32 9, i32 13>
  store <4 x i16> %4, ptr %1, align 8
  ret void
}