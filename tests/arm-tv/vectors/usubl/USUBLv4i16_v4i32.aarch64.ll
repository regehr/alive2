define <4 x i32> @f(<4 x i16> %0, <4 x i16> %1) {
  %3 = zext <4 x i16> %0 to <4 x i32>
  %4 = zext <4 x i16> %1 to <4 x i32>
  %5 = sub <4 x i32> %3, %4
  %6 = and <4 x i32> %5, <i32 65535, i32 65535, i32 65535, i32 65535>
  ret <4 x i32> %6
}