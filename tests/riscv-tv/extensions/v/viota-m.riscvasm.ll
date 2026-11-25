; viota.m: element i is the number of set mask bits below i
define void @test(ptr %dst, ptr %src) {
  %a0 = load i8, ptr %src, align 1
  %p1 = getelementptr i8, ptr %src, i64 1
  %a1 = load i8, ptr %p1, align 1
  %p2 = getelementptr i8, ptr %src, i64 2
  %a2 = load i8, ptr %p2, align 1
  %p3 = getelementptr i8, ptr %src, i64 3
  %a3 = load i8, ptr %p3, align 1
  %n0 = icmp ne i8 %a0, 0
  %n1 = icmp ne i8 %a1, 0
  %n2 = icmp ne i8 %a2, 0
  %c0 = zext i1 %n0 to i8
  %c1 = zext i1 %n1 to i8
  %c2 = zext i1 %n2 to i8
  %s1 = add i8 %c0, %c1
  %s2 = add i8 %s1, %c2
  store i8 0, ptr %dst, align 1
  %d1 = getelementptr i8, ptr %dst, i64 1
  store i8 %c0, ptr %d1, align 1
  %d2 = getelementptr i8, ptr %dst, i64 2
  store i8 %s1, ptr %d2, align 1
  %d3 = getelementptr i8, ptr %dst, i64 3
  store i8 %s2, ptr %d3, align 1
  ret void
}
