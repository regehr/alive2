; the immediate of an unsigned compare is still sign-extended, so this
; compares against 0xfff0
define void @test(ptr %dst, ptr %src) {
  %a = load i16, ptr %src, align 2
  %c = icmp ule i16 %a, 65520
  %r = select i1 %c, i16 7, i16 %a
  store i16 %r, ptr %dst, align 2
  ret void
}
