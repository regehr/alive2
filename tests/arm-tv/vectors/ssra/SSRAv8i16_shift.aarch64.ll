; Function Attrs: nounwind
define <8 x i16> @f(ptr %0, ptr %1) {
  %3 = load <8 x i16>, ptr %0, align 16
  %4 = ashr <8 x i16> %3, <i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1, i16 1>
  %5 = load <8 x i16>, ptr %1, align 16
  %6 = add <8 x i16> %4, %5
  ret <8 x i16> %6
}