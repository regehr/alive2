; CHECK: 1 correct transformations

; Preserve the exact FP bits in a1 when an integer argument occupies a0.
define i64 @test(i64 %integer, float %f0, float %f1, float %f2, float %f3,
                 float %f4, float %f5, float %f6, float %f7,
                 float noundef %value) {
  %bits = bitcast float %value to i32
  %wide = zext i32 %bits to i64
  %result = xor i64 %integer, %wide
  ret i64 %result
}
