; inactive elements take no part in a reduction; bits 0 and 1 of %m are
; the mask
define i64 @test(ptr %src, i64 %m) {
  %a0 = load i8, ptr %src, align 1
  %p1 = getelementptr i8, ptr %src, i64 1
  %a1 = load i8, ptr %p1, align 1
  %b0 = and i64 %m, 1
  %b1 = and i64 %m, 2
  %c0 = icmp ne i64 %b0, 0
  %c1 = icmp ne i64 %b1, 0
  %e0 = select i1 %c0, i8 %a0, i8 0
  %e1 = select i1 %c1, i8 %a1, i8 0
  %mx = call i8 @llvm.umax.i8(i8 %e0, i8 %e1)
  %r = sext i8 %mx to i64
  ret i64 %r
}
declare i8 @llvm.umax.i8(i8, i8)
