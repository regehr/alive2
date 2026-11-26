define <4 x float> @test(<4 x i32> %x) {
  %r = uitofp <4 x i32> %x to <4 x float>
  ret <4 x float> %r
}
