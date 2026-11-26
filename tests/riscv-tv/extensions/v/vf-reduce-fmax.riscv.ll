define double @test(<4 x double> %x) {
  %r = call double @llvm.vector.reduce.fmax.v4f64(<4 x double> %x)
  ret double %r
}
