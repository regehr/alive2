; Function Attrs: nounwind
define <16 x i8> @xtn2_16b(<8 x i8> %0, <8 x i16> %1) {
  %3 = trunc <8 x i16> %1 to <8 x i8>
  %4 = shufflevector <8 x i8> %0, <8 x i8> %3, <16 x i32> <i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7, i32 8, i32 9, i32 10, i32 11, i32 12, i32 13, i32 14, i32 15>
  ret <16 x i8> %4
}