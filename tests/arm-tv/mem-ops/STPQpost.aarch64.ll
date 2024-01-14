; Function Attrs: nounwind
define <8 x i16> @f(<8 x i16> %0, <8 x i16> %1) {
  %3 = alloca <8 x i16>, align 16
  %4 = alloca <8 x i16>, align 16
  store <8 x i16> %0, ptr %3, align 16
  store <8 x i16> %1, ptr %4, align 16
  %5 = load <8 x i16>, ptr %3, align 16
  %6 = load <8 x i16>, ptr %4, align 16
  %7 = lshr <8 x i16> %5, %6
  ret <8 x i16> %7
}