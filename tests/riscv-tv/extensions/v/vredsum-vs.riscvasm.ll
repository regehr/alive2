; vredsum.vs adds the scalar in vs1[0] to the body elements of vs2
define i64 @test(ptr %src, i64 %x) {
  %a0 = load i32, ptr %src, align 4
  %p1 = getelementptr i8, ptr %src, i64 4
  %a1 = load i32, ptr %p1, align 4
  %p2 = getelementptr i8, ptr %src, i64 8
  %a2 = load i32, ptr %p2, align 4
  %p3 = getelementptr i8, ptr %src, i64 12
  %a3 = load i32, ptr %p3, align 4
  %t = trunc i64 %x to i32
  %s0 = add i32 %t, %a0
  %s1 = add i32 %s0, %a1
  %s2 = add i32 %s1, %a2
  %s3 = add i32 %s2, %a3
  %r = sext i32 %s3 to i64
  ret i64 %r
}
