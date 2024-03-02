; Function Attrs: nounwind
define void @f(<4 x i16> %0, <4 x i16> %1, ptr %2) {
  %4 = and <4 x i16> %0, <i16 16383, i16 16383, i16 16383, i16 16383>
  %5 = shl <4 x i16> %1, <i16 14, i16 14, i16 14, i16 14>
  %6 = or <4 x i16> %4, %5
  store <4 x i16> %6, ptr %2, align 8
  ret void
}