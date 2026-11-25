; vsext.vf4 at SEW = 32 reads an EEW = 8, EMUL = 1/4 source
define void @test(ptr %dst, ptr %src) {
  %a0 = load i8, ptr %src, align 1
  %ps1 = getelementptr i8, ptr %src, i64 1
  %a1 = load i8, ptr %ps1, align 1
  %r0 = sext i8 %a0 to i32
  %r1 = sext i8 %a1 to i32
  store i32 %r0, ptr %dst, align 4
  %p1 = getelementptr i8, ptr %dst, i64 4
  store i32 %r1, ptr %p1, align 4
  ret void
}
