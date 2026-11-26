; CHECK: ERROR: unordered floating-point reductions are not supported
define float @test(float %s, <2 x float> %x) {
  %x0 = extractelement <2 x float> %x, i64 0
  %x1 = extractelement <2 x float> %x, i64 1
  %a = fadd float %s, %x0
  %r = fadd float %a, %x1
  ret float %r
}
