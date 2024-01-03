define <2 x i64> @vextsw2dLE(<4 x i32> %0) {
  %2 = extractelement <4 x i32> %0, i32 0
  %3 = sext i32 %2 to i64
  %4 = insertelement <2 x i64> undef, i64 %3, i32 0
  %5 = extractelement <4 x i32> %0, i32 2
  %6 = sext i32 %5 to i64
  %7 = insertelement <2 x i64> %4, i64 %6, i32 1
  ret <2 x i64> %7
}