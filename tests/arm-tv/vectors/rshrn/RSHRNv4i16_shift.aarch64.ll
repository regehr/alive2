define <4 x i16> @f(<4 x i32> %0) {
  %2 = add <4 x i32> %0, <i32 8, i32 8, i32 8, i32 8>
  %3 = lshr <4 x i32> %2, <i32 4, i32 4, i32 4, i32 4>
  %4 = trunc <4 x i32> %3 to <4 x i16>
  ret <4 x i16> %4
}