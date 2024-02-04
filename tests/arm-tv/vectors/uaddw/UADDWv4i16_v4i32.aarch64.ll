define <4 x i32> @f(<4 x i32> %0, <4 x i16> %1) {
  %3 = zext <4 x i16> %1 to <4 x i32>
  %4 = add <4 x i32> %0, %3
  ret <4 x i32> %4
}