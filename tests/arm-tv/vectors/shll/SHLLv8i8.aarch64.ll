; Function Attrs: nounwind
define <8 x i16> @f(ptr %0) {
  %2 = load <8 x i8>, ptr %0, align 8
  %3 = sext <8 x i8> %2 to <8 x i16>
  %4 = shl <8 x i16> %3, <i16 8, i16 8, i16 8, i16 8, i16 8, i16 8, i16 8, i16 8>
  ret <8 x i16> %4
}