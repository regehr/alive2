; vrsub.vi computes imm - vs2[i], with the immediate sign-extended
define void @test(ptr %dst, ptr %src) {
  %a0 = load i16, ptr %src, align 2
  %ps1 = getelementptr i8, ptr %src, i64 2
  %a1 = load i16, ptr %ps1, align 2
  %r0 = sub i16 -3, %a0
  %r1 = sub i16 -3, %a1
  store i16 %r0, ptr %dst, align 2
  %p1 = getelementptr i8, ptr %dst, i64 2
  store i16 %r1, ptr %p1, align 2
  ret void
}
