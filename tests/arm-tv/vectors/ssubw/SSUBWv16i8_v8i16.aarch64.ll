define <8 x i16> @f(<8 x i16> %0, <16 x i8> %1) {
  %3 = shufflevector <16 x i8> %1, <16 x i8> undef, <8 x i32> <i32 8, i32 9, i32 10, i32 11, i32 12, i32 13, i32 14, i32 15>
  %4 = sext <8 x i8> %3 to <8 x i16>
  %5 = sub <8 x i16> %0, %4
  ret <8 x i16> %5
}