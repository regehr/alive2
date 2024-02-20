define <4 x i32> @f(<8 x i16> %0) {
  %2 = shufflevector <8 x i16> %0, <8 x i16> undef, <4 x i32> <i32 4, i32 5, i32 6, i32 7>
  %3 = zext <4 x i16> %2 to <4 x i32>
  %4 = shl <4 x i32> %3, <i32 16, i32 16, i32 16, i32 16>
  ret <4 x i32> %4
}