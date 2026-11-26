; vfmacc.vf vd, fs1, vs2: vd = +(fs1 * vs2) + vd
define <2 x float> @test(float %s, <2 x float> %x, <2 x float> %z) {
  %h = insertelement <2 x float> poison, float %s, i64 0
  %sp = shufflevector <2 x float> %h, <2 x float> poison, <2 x i32> zeroinitializer
  %r = call <2 x float> @llvm.fma.v2f32(<2 x float> %sp, <2 x float> %x, <2 x float> %z)
  ret <2 x float> %r
}
