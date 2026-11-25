; offsets narrower than XLEN are zero-extended; here 8-bit offsets index
; 64-bit data, so the offsets have EMUL = 1/8
define void @test(ptr %dst, ptr %base, ptr %offs) {
  %o0 = load i8, ptr %offs, align 1
  %q1 = getelementptr i8, ptr %offs, i64 1
  %o1 = load i8, ptr %q1, align 1
  %w0 = zext i8 %o0 to i64
  %w1 = zext i8 %o1 to i64
  %p0 = getelementptr i8, ptr %base, i64 %w0
  %p1 = getelementptr i8, ptr %base, i64 %w1
  %a0 = load i64, ptr %p0, align 8
  %a1 = load i64, ptr %p1, align 8
  store i64 %a0, ptr %dst, align 8
  %d1 = getelementptr i8, ptr %dst, i64 8
  store i64 %a1, ptr %d1, align 8
  ret void
}
