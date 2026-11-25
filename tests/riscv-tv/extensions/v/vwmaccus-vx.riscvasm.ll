; vwmaccus.vx vd, rs1, vs2: 2*SEW vd[i] += unsigned(x[rs1]) * signed(vs2[i])
define void @test(ptr %dst, ptr %pd, ptr %pb, i64 %x) {
  %d = load i16, ptr %pd, align 2
  %b = load i8, ptr %pb, align 1
  %t = trunc i64 %x to i8
  %wt = zext i8 %t to i16
  %wb = sext i8 %b to i16
  %m = mul i16 %wt, %wb
  %r = add i16 %m, %d
  store i16 %r, ptr %dst, align 2
  ret void
}
