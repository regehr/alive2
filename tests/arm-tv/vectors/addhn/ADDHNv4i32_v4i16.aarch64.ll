define <4 x i16> @f(<4 x i32> %0, <4 x i32> %1) {
  %3 = add <4 x i32> %0, %1
  %4 = lshr <4 x i32> %3, <i32 16, i32 16, i32 16, i32 16>
  %5 = trunc <4 x i32> %4 to <4 x i16>
  ret <4 x i16> %5
}