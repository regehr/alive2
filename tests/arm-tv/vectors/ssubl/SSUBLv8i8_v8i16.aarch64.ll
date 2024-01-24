; Function Attrs: nounwind
define <8 x i16> @f(ptr %0, ptr %1) {
  %3 = load <8 x i8>, ptr %0, align 8
  %4 = load <8 x i8>, ptr %1, align 8
  %5 = sext <8 x i8> %3 to <8 x i16>
  %6 = sext <8 x i8> %4 to <8 x i16>
  %7 = sub <8 x i16> %5, %6
  ret <8 x i16> %7
}