define <16 x i8> @f2(<16 x i8> %0, ptr %1) {
  %3 = load i8, ptr %1, align 1
  %4 = insertelement <16 x i8> %0, i8 %3, i32 15
  ret <16 x i8> %4
}