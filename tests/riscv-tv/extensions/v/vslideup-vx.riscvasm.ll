; vslideup.vx with a variable offset: body elements below the offset are
; left unchanged, even under ta/ma
define void @test(ptr %dst, ptr %src, i64 %x) {
  %d0 = load i8, ptr %dst, align 1
  %pd1 = getelementptr i8, ptr %dst, i64 1
  %d1 = load i8, ptr %pd1, align 1
  %pd2 = getelementptr i8, ptr %dst, i64 2
  %d2 = load i8, ptr %pd2, align 1
  %pd3 = getelementptr i8, ptr %dst, i64 3
  %d3 = load i8, ptr %pd3, align 1
  %s0 = load i8, ptr %src, align 1
  %ps1 = getelementptr i8, ptr %src, i64 1
  %s1 = load i8, ptr %ps1, align 1
  %ps2 = getelementptr i8, ptr %src, i64 2
  %s2 = load i8, ptr %ps2, align 1
  %ps3 = getelementptr i8, ptr %src, i64 3
  %s3 = load i8, ptr %ps3, align 1
  %x0 = icmp eq i64 %x, 0
  %x1 = icmp eq i64 %x, 1
  %x2 = icmp eq i64 %x, 2
  %x3 = icmp eq i64 %x, 3
  %r0 = select i1 %x0, i8 %s0, i8 %d0
  %r1a = select i1 %x1, i8 %s0, i8 %d1
  %r1 = select i1 %x0, i8 %s1, i8 %r1a
  %r2a = select i1 %x2, i8 %s0, i8 %d2
  %r2b = select i1 %x1, i8 %s1, i8 %r2a
  %r2 = select i1 %x0, i8 %s2, i8 %r2b
  %r3a = select i1 %x3, i8 %s0, i8 %d3
  %r3b = select i1 %x2, i8 %s1, i8 %r3a
  %r3c = select i1 %x1, i8 %s2, i8 %r3b
  %r3 = select i1 %x0, i8 %s3, i8 %r3c
  store i8 %r0, ptr %dst, align 1
  store i8 %r1, ptr %pd1, align 1
  store i8 %r2, ptr %pd2, align 1
  store i8 %r3, ptr %pd3, align 1
  ret void
}
