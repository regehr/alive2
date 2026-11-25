; CHECK: 1 incorrect transformations
; vmadd overwrites a multiplicand: it is vs1 * vd + vs2, not the vmacc
; computation vs1 * vs2 + vd
define void @test(ptr %dst, ptr %pd, ptr %p1, ptr %p2) {
  %d = load i8, ptr %pd, align 1
  %s1 = load i8, ptr %p1, align 1
  %s2 = load i8, ptr %p2, align 1
  %m = mul i8 %s1, %s2
  %r = add i8 %m, %d
  store i8 %r, ptr %dst, align 1
  ret void
}
