; a masked compare may write v0, and under mu it ANDs in the mask:
; (a < b) && (b < c), from the example in the ISA manual
define void @test(ptr %dst, ptr %pa, ptr %pb, ptr %pc) {
  %a0 = load i8, ptr %pa, align 1
  %b0 = load i8, ptr %pb, align 1
  %c0 = load i8, ptr %pc, align 1
  %qa = getelementptr i8, ptr %pa, i64 1
  %qb = getelementptr i8, ptr %pb, i64 1
  %qc = getelementptr i8, ptr %pc, i64 1
  %a1 = load i8, ptr %qa, align 1
  %b1 = load i8, ptr %qb, align 1
  %c1 = load i8, ptr %qc, align 1
  %x0 = icmp slt i8 %a0, %b0
  %y0 = icmp slt i8 %b0, %c0
  %x1 = icmp slt i8 %a1, %b1
  %y1 = icmp slt i8 %b1, %c1
  %t0 = and i1 %x0, %y0
  %t1 = and i1 %x1, %y1
  %r0 = zext i1 %t0 to i8
  %r1 = zext i1 %t1 to i8
  store i8 %r0, ptr %dst, align 1
  %p1 = getelementptr i8, ptr %dst, i64 1
  store i8 %r1, ptr %p1, align 1
  ret void
}
