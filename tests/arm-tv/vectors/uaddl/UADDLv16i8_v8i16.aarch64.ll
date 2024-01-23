define <8 x i16> @f(<16 x i8> %0, <16 x i8> %1) {
  %3 = shufflevector <16 x i8> %0, <16 x i8> undef, <8 x i32> <i32 8, i32 9, i32 10, i32 11, i32 12, i32 13, i32 14, i32 15>
  %4 = zext <8 x i8> %3 to <8 x i16>
  %5 = shufflevector <16 x i8> %1, <16 x i8> undef, <8 x i32> <i32 8, i32 9, i32 10, i32 11, i32 12, i32 13, i32 14, i32 15>
  %6 = zext <8 x i8> %5 to <8 x i16>
  %7 = add <8 x i16> %4, %6
  ret <8 x i16> %7
}