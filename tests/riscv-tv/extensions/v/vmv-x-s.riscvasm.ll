; vmv.x.s sign-extends element 0 to XLEN
define i64 @test(ptr %src) {
  %a = load i16, ptr %src, align 2
  %r = sext i16 %a to i64
  ret i64 %r
}
