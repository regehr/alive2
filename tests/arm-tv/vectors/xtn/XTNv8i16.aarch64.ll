; Function Attrs: nounwind
define <8 x i16> @xtn2_8h(<4 x i16> %0, <4 x i32> %1) {
  %3 = trunc <4 x i32> %1 to <4 x i16>
  %4 = shufflevector <4 x i16> %0, <4 x i16> %3, <8 x i32> <i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7>
  ret <8 x i16> %4
}