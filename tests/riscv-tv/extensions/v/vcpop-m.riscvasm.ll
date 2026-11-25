; vcpop.m counts the set body elements of a mask: here, the zero bytes
; among the first four
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
  %c0 = zext i1 %z0 to i64
  %c1 = zext i1 %z1 to i64
  %c2 = zext i1 %z2 to i64
  %c3 = zext i1 %z3 to i64
  %s01 = add i64 %c0, %c1
  %s23 = add i64 %c2, %c3
  %s = add i64 %s01, %s23
  ret i64 %s
}
