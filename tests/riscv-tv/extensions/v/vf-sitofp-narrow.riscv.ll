define <2 x float> @test(<2 x i64> %x) {
  %r = sitofp <2 x i64> %x to <2 x float>
  ret <2 x float> %r
}
