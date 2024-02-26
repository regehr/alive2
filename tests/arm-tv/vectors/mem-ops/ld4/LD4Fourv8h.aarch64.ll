; Function Attrs: nounwind
define void @f(ptr %0, ptr %1) {
  %3 = load <32 x i16>, ptr %0, align 64
  %4 = shufflevector <32 x i16> %3, <32 x i16> undef, <8 x i32> <i32 1, i32 5, i32 9, i32 13, i32 17, i32 21, i32 25, i32 29>
  store <8 x i16> %4, ptr %1, align 16
  ret void
}