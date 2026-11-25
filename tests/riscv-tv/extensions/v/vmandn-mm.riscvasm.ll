; vmandn.mm vd, vs2, vs1 computes vs2 && !vs1
define void @test(ptr %dst, ptr %pa, ptr %pb) {
  %a0 = load i8, ptr %pa, align 1
  %b0 = load i8, ptr %pb, align 1
  %qa = getelementptr i8, ptr %pa, i64 1
  %qb = getelementptr i8, ptr %pb, i64 1
  %a1 = load i8, ptr %qa, align 1
  %b1 = load i8, ptr %qb, align 1
  %za0 = icmp eq i8 %a0, 0
  %zb0 = icmp ne i8 %b0, 0
  %za1 = icmp eq i8 %a1, 0
  %zb1 = icmp ne i8 %b1, 0
  %t0 = and i1 %za0, %zb0
  %t1 = and i1 %za1, %zb1
  %r0 = zext i1 %t0 to i8
  %r1 = zext i1 %t1 to i8
  store i8 %r0, ptr %dst, align 1
  %p1 = getelementptr i8, ptr %dst, i64 1
  store i8 %r1, ptr %p1, align 1
  ret void
}
