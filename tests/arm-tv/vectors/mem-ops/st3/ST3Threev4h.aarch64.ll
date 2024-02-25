; Function Attrs: nounwind
define void @f(<8 x i16> %0, <8 x i16> %1, ptr %2) {
  %4 = shufflevector <8 x i16> %0, <8 x i16> %1, <12 x i32> <i32 0, i32 4, i32 8, i32 1, i32 5, i32 9, i32 2, i32 6, i32 10, i32 3, i32 7, i32 11>
  store <12 x i16> %4, ptr %2, align 32
  ret void
}