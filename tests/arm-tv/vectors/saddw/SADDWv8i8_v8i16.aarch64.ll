define <4 x i16> @f(<4 x i16> %0, <8 x i8> %1) {
  %3 = sext <8 x i8> %1 to <8 x i16>
  %4 = shufflevector <8 x i16> %3, <8 x i16> poison, <4 x i32> <i32 0, i32 1, i32 2, i32 3>
  %5 = add <4 x i16> %0, %4
  ret <4 x i16> %5
}