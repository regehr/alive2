; CHECK: 1 incorrect transformations
; vmadd overwrites a multiplicand: it is vs1 * vd + vs2, not the vmacc
; computation vs1 * vs2 + vd
define void @test(ptr %dst, ptr %pd, ptr %p1, ptr %p2) {
  %d = load i16, ptr %pd, align 2
  %s1 = load i16, ptr %p1, align 2
  %s2 = load i16, ptr %p2, align 2
  %m = mul i16 %s1, %s2
  %r = add i16 %m, %d
  store i16 %r, ptr %dst, align 2
  ret void
}
