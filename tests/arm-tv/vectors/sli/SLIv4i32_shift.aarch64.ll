; Function Attrs: nounwind
define void @f(<4 x i32> %0, <4 x i32> %1, ptr %2) {
  %4 = and <4 x i32> %0, <i32 4194303, i32 4194303, i32 4194303, i32 4194303>
  %5 = shl <4 x i32> %1, <i32 22, i32 22, i32 22, i32 22>
  %6 = or <4 x i32> %4, %5
  store <4 x i32> %6, ptr %2, align 16
  ret void
}