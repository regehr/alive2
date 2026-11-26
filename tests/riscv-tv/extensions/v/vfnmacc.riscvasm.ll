; vfnmacc.vv vd, vs1, vs2: vd = -(vs1 * vs2) - vd
define <2 x float> @test(<2 x float> %x, <2 x float> %y, <2 x float> %z) {
  %nx = fneg <2 x float> %x
  %nz = fneg <2 x float> %z
  %r = call <2 x float> @llvm.fma.v2f32(<2 x float> %nx, <2 x float> %y, <2 x float> %nz)
  ret <2 x float> %r
}
