; vnsrl.wi narrows a 2*SEW source: here, the high byte of each halfword
define void @test(ptr %dst, ptr %src) {
  %a0 = load i16, ptr %src, align 2
  %ps1 = getelementptr i8, ptr %src, i64 2
  %a1 = load i16, ptr %ps1, align 2
  %s0 = lshr i16 %a0, 8
  %s1 = lshr i16 %a1, 8
  %r0 = trunc i16 %s0 to i8
  %r1 = trunc i16 %s1 to i8
  store i8 %r0, ptr %dst, align 1
  %p1 = getelementptr i8, ptr %dst, i64 1
  store i8 %r1, ptr %p1, align 1
  ret void
}
