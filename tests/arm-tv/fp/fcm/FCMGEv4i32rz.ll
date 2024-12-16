define <2 x i64> @f(<4 x float> %0, <4 x float> %1) {
  %3 = fcmp ult <4 x float> %0, zeroinitializer
  %4 = sext <4 x i1> %3 to <4 x i32>
  %5 = fcmp ult <4 x float> %1, zeroinitializer
  %6 = sext <4 x i1> %5 to <4 x i32>
  %7 = or <4 x i32> %4, %6
  %8 = bitcast <4 x i32> %7 to <2 x i64>
  ret <2 x i64> %8
}
