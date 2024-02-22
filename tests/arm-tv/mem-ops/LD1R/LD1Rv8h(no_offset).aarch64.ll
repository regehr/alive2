define <8 x i16> @f5(ptr %0) {
  %2 = load i16, ptr %0, align 2
  %3 = insertelement <8 x i16> undef, i16 %2, i32 3
  ret <8 x i16> %3
}