; vwmulsu: the full product of signed(vs2) and unsigned(rs1)
define void @test(ptr %dst, ptr %src, i64 %x) {
  %a = load i8, ptr %src, align 1
  %t = trunc i64 %x to i8
  %wa = sext i8 %a to i16
  %wb = zext i8 %t to i16
  %r = mul i16 %wa, %wb
  store i16 %r, ptr %dst, align 2
  ret void
}
