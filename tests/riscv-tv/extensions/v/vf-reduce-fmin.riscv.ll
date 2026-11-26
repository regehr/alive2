declare float @llvm.vector.reduce.fminimum.v2f32(<2 x float>)
define float @test(<2 x float> %x) {
  %r = call float @llvm.vector.reduce.fminimum.v2f32(<2 x float> %x)
  ret float %r
}
