define <4 x i32> @f(<4 x float> %0) {
  %2 = fcmp ule <4 x float> %0, zeroinitializer
  %3 = sext <4 x i1> %2 to <4 x i32>
  ret <4 x i32> %3
}
