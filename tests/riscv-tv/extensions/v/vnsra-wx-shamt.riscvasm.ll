; a narrowing shift uses the low lg2(2*SEW) bits of the amount. the
; destination may share the lowest register of the wider source
define void @test(ptr %dst, ptr %src, i64 %x) {
  %a = load i16, ptr %src, align 2
  %t = trunc i64 %x to i16
  %m = and i16 %t, 15
  %s = ashr i16 %a, %m
  %r = trunc i16 %s to i8
  store i8 %r, ptr %dst, align 1
  ret void
}
