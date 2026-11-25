; vslidedown reads zero at and past VLMAX, which is 4 here; the offset is
; all 64 bits of the register
define i64 @test(ptr %src, i64 %x) {
  %a0 = load i32, ptr %src, align 4
  %p1 = getelementptr i8, ptr %src, i64 4
  %a1 = load i32, ptr %p1, align 4
  %p2 = getelementptr i8, ptr %src, i64 8
  %a2 = load i32, ptr %p2, align 4
  %p3 = getelementptr i8, ptr %src, i64 12
  %a3 = load i32, ptr %p3, align 4
  %is0 = icmp eq i64 %x, 0
  %is1 = icmp eq i64 %x, 1
  %is2 = icmp eq i64 %x, 2
  %is3 = icmp eq i64 %x, 3
  %s3 = select i1 %is3, i32 %a3, i32 0
  %s2 = select i1 %is2, i32 %a2, i32 %s3
  %s1 = select i1 %is1, i32 %a1, i32 %s2
  %s0 = select i1 %is0, i32 %a0, i32 %s1
  %r = sext i32 %s0 to i64
  ret i64 %r
}
