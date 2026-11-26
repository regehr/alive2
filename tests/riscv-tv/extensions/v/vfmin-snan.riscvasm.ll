; vfmin returns the other operand when one is NaN, and a canonical NaN when
; both are
declare <2 x float> @llvm.minimumnum.v2f32(<2 x float>, <2 x float>)
define <2 x float> @test(<2 x float> %x, float %f) {
  %i = insertelement <2 x float> poison, float %f, i64 0
  %sp = shufflevector <2 x float> %i, <2 x float> poison, <2 x i32> zeroinitializer
  %r = call <2 x float> @llvm.minimumnum.v2f32(<2 x float> %x, <2 x float> %sp)
  ret <2 x float> %r
}
