; vmulhsu: the high half of signed(vs2) * unsigned(rs1)
define void @test(ptr %dst, ptr %src, i64 %x) {
  %a = load i8, ptr %src, align 1
  %b = trunc i64 %x to i8
  %wa = sext i8 %a to i16
  %wb = zext i8 %b to i16
  %p = mul i16 %wa, %wb
  %h = lshr i16 %p, 8
  %r = trunc i16 %h to i8
  store i8 %r, ptr %dst, align 1
  ret void
}
