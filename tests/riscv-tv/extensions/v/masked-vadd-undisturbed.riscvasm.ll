; bits 0 and 1 of %m are the mask; with mu, inactive elements keep their
; old value
define void @test(ptr %dst, ptr %src, i64 %m) {
  %a0 = load i8, ptr %src, align 1
  %ps1 = getelementptr i8, ptr %src, i64 1
  %a1 = load i8, ptr %ps1, align 1
  %b0 = and i64 %m, 1
  %b1 = and i64 %m, 2
  %c0 = icmp ne i64 %b0, 0
  %c1 = icmp ne i64 %b1, 0
  %s0 = add i8 %a0, 1
  %s1 = add i8 %a1, 1
  %r0 = select i1 %c0, i8 %s0, i8 %a0
  %r1 = select i1 %c1, i8 %s1, i8 %a1
  store i8 %r0, ptr %dst, align 1
  %p1 = getelementptr i8, ptr %dst, i64 1
  store i8 %r1, ptr %p1, align 1
  ret void
}
