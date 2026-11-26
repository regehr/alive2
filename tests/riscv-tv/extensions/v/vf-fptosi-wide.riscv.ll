define <2 x i64> @test(<2 x float> %x) {
  %r = fptosi <2 x float> %x to <2 x i64>
  ret <2 x i64> %r
}
