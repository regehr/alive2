; CHECK: 1 incorrect transformations
; vmerge takes the second source where the mask is set, not vs2
define void @test(ptr %dst, ptr %src) {
  %a0 = load i16, ptr %src, align 2
  %ps1 = getelementptr i8, ptr %src, i64 2
  %a1 = load i16, ptr %ps1, align 2
  %z0 = icmp eq i16 %a0, 0
  %z1 = icmp eq i16 %a1, 0
  %r0 = select i1 %z0, i16 %a0, i16 7
  %r1 = select i1 %z1, i16 %a1, i16 7
  store i16 %r0, ptr %dst, align 2
  %p1 = getelementptr i8, ptr %dst, i64 2
  store i16 %r1, ptr %p1, align 2
  ret void
}
