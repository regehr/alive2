define <2 x float> @f(<2 x float> %0) {
  %2 = fcmp ogt <2 x float> %0, zeroinitializer
  %3 = fneg <2 x float> %0
  %4 = select nnan nsz <2 x i1> %2, <2 x float> %0, <2 x float> %3
  ret <2 x float> %4
}
