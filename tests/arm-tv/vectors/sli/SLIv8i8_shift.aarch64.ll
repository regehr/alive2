; Function Attrs: nounwind
define void @f(<8 x i8> %0, <8 x i8> %1, ptr %2) {
  %4 = and <8 x i8> %0, <i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7>
  %5 = shl <8 x i8> %1, <i8 3, i8 3, i8 3, i8 3, i8 3, i8 3, i8 3, i8 3>
  %6 = or <8 x i8> %4, %5
  store <8 x i8> %6, ptr %2, align 8
  ret void
}