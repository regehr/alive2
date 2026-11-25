; vmacc.vx vd, rs1, vs2: vd[i] = x[rs1] * vs2[i] + vd[i]
define void @test(ptr %dst, ptr %pd, ptr %pb, i64 %x) {
  %d0 = load i16, ptr %pd, align 2
  %b0 = load i16, ptr %pb, align 2
  %qd = getelementptr i8, ptr %pd, i64 2
  %qb = getelementptr i8, ptr %pb, i64 2
  %d1 = load i16, ptr %qd, align 2
  %b1 = load i16, ptr %qb, align 2
  %t = trunc i64 %x to i16
  %m0 = mul i16 %t, %b0
  %m1 = mul i16 %t, %b1
  %r0 = add i16 %m0, %d0
  %r1 = add i16 %m1, %d1
  store i16 %r0, ptr %dst, align 2
  %p1 = getelementptr i8, ptr %dst, i64 2
  store i16 %r1, ptr %p1, align 2
  ret void
}
