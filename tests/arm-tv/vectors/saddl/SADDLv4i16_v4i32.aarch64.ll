define <4 x i32> @f(<4 x i16> %0, <4 x i16> %1) {
  %3 = sext <4 x i16> %0 to <4 x i32>
  %4 = sext <4 x i16> %1 to <4 x i32>
  %5 = add <4 x i32> %3, %4
  ret <4 x i32> %5
}