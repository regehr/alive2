define <2 x i32> @f(<2 x float> %0) {
  %2 = fcmp ule <2 x float> %0, zeroinitializer
  %3 = sext <2 x i1> %2 to <2 x i32>
  ret <2 x i32> %3
}
