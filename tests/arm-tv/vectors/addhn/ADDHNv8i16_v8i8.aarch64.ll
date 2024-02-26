; Function Attrs: nounwind
define <8 x i8> @f(ptr %0, ptr %1) {
  %3 = load <8 x i16>, ptr %0, align 16
  %4 = load <8 x i16>, ptr %1, align 16
  %5 = add <8 x i16> %3, %4
  %6 = lshr <8 x i16> %5, <i16 8, i16 8, i16 8, i16 8, i16 8, i16 8, i16 8, i16 8>
  %7 = trunc <8 x i16> %6 to <8 x i8>
  ret <8 x i8> %7
}