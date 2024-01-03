define <2 x i64> @vextsb2dLE(<16 x i8> %0) {
  %2 = extractelement <16 x i8> %0, i32 0
  %3 = sext i8 %2 to i64
  %4 = insertelement <2 x i64> undef, i64 %3, i32 0
  %5 = extractelement <16 x i8> %0, i32 8
  %6 = sext i8 %5 to i64
  %7 = insertelement <2 x i64> %4, i64 %6, i32 1
  ret <2 x i64> %7
}