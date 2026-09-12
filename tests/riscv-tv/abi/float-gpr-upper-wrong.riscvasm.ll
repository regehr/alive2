; CHECK: 1 incorrect transformations

; After fa0-fa7 are exhausted, the remaining GPR bits are not NaN-boxed.
define i64 @test(float %f0, float %f1, float %f2, float %f3,
                 float %f4, float %f5, float %f6, float %f7,
                 float noundef %value) {
  ret i64 4294967295
}
