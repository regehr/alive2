; Function Attrs: nounwind
define void @f(<16 x i8> %0, <16 x i8> %1, ptr %2) {
  %4 = and <16 x i8> %0, <i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7>
  %5 = shl <16 x i8> %1, <i8 3, i8 3, i8 3, i8 3, i8 3, i8 3, i8 3, i8 3, i8 3, i8 3, i8 3, i8 3, i8 3, i8 3, i8 3, i8 3>
  %6 = or <16 x i8> %4, %5
  store <16 x i8> %6, ptr %2, align 16
  ret void
}