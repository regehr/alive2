; counting the bits of vmsbf gives the index of the first zero byte among
; the first four, or 4 if there is none
define i64 @test(ptr %src) {
  %a0 = load i8, ptr %src, align 1
  %p1 = getelementptr i8, ptr %src, i64 1
  %a1 = load i8, ptr %p1, align 1
  %p2 = getelementptr i8, ptr %src, i64 2
  %a2 = load i8, ptr %p2, align 1
  %p3 = getelementptr i8, ptr %src, i64 3
  %a3 = load i8, ptr %p3, align 1
  %z0 = icmp eq i8 %a0, 0
  %z1 = icmp eq i8 %a1, 0
  %z2 = icmp eq i8 %a2, 0
  %z3 = icmp eq i8 %a3, 0
  %r3 = select i1 %z3, i64 3, i64 4
  %r2 = select i1 %z2, i64 2, i64 %r3
  %r1 = select i1 %z1, i64 1, i64 %r2
  %r0 = select i1 %z0, i64 0, i64 %r1
  ret i64 %r0
}
