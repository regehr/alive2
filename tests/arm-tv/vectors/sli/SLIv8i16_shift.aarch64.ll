; Function Attrs: nounwind
define void @f(<8 x i16> %0, <8 x i16> %1, ptr %2) {
  %4 = and <8 x i16> %0, <i16 16383, i16 16383, i16 16383, i16 16383, i16 16383, i16 16383, i16 16383, i16 16383>
  %5 = shl <8 x i16> %1, <i16 14, i16 14, i16 14, i16 14, i16 14, i16 14, i16 14, i16 14>
  %6 = or <8 x i16> %4, %5
  store <8 x i16> %6, ptr %2, align 16
  ret void
}