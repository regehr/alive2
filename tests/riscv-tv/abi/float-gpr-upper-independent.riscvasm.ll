; CHECK: 1 incorrect transformations

; Different arguments have independent excess bits in their GPRs.
define i64 @test(float %f0, float %f1, float %f2, float %f3,
                 float %f4, float %f5, float %f6, float %f7,
                 float noundef %first, float noundef %second) {
  ret i64 0
}
