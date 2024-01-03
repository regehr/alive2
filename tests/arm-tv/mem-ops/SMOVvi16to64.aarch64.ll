define <2 x i64> @vextsh2dLE(<8 x i16> %0) {
  %2 = extractelement <8 x i16> %0, i32 0
  %3 = sext i16 %2 to i64
  %4 = insertelement <2 x i64> undef, i64 %3, i32 0
  %5 = extractelement <8 x i16> %0, i32 4
  %6 = sext i16 %5 to i64
  %7 = insertelement <2 x i64> %4, i64 %6, i32 1
  ret <2 x i64> %7
}