; vfwredosum widens each active element and adds them in order to vs1[0];
; with vl = 1 only x[0] is added
define double @test(double %s, <2 x float> %x) {
  %x0 = extractelement <2 x float> %x, i64 0
  %e = fpext float %x0 to double
  %r = fadd double %s, %e
  ret double %r
}
