; the c.ntl.* and ntl.* non-temporal hints (c.add / add with rd = x0)
; have no architectural effect
define i64 @test(i64 %x) {
  ret i64 %x
}
