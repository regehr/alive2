declare <2 x float> @llvm.maximumnum.v2f32(<2 x float>, <2 x float>)
define <2 x float> @test(<2 x float> %x, <2 x float> %y) {
  %r = call <2 x float> @llvm.maximumnum.v2f32(<2 x float> %x, <2 x float> %y)
  ret <2 x float> %r
}
