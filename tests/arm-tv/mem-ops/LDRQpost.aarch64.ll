define <16 x i8> @f(<16 x i8> %0, i8 %1, i32 %2) {
  %4 = insertelement <16 x i8> %0, i8 %1, i32 %2
  ret <16 x i8> %4
}