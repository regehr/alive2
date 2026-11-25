; vwadd.wx: a 2*SEW vs2 plus the sign-extended low SEW bits of the scalar.
; vd and vs2 have the same EEW, so they may be the same group
define void @test(ptr %dst, ptr %src, i64 %x) {
  %a0 = load i16, ptr %src, align 2
  %ps1 = getelementptr i8, ptr %src, i64 2
  %a1 = load i16, ptr %ps1, align 2
  %t = trunc i64 %x to i8
  %w = sext i8 %t to i16
  %r0 = add i16 %a0, %w
  %r1 = add i16 %a1, %w
  store i16 %r0, ptr %dst, align 2
  %p1 = getelementptr i8, ptr %dst, i64 2
  store i16 %r1, ptr %p1, align 2
  ret void
}
