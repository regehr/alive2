define <2 x i1> @test(<2 x float> %x, <2 x float> %y) {
  %c = fcmp une <2 x float> %x, %y
  ret <2 x i1> %c
}
