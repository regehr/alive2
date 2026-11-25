; the shift immediate is zero-extended: at SEW = 64, a sign-extended 31
; would shift by 63
define void @test(ptr %dst, ptr %src) {
  %a = load i64, ptr %src, align 8
  %r = lshr i64 %a, 31
  store i64 %r, ptr %dst, align 8
  ret void
}
