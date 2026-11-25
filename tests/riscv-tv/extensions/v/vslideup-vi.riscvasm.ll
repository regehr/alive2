; vslideup leaves the elements below the offset unchanged
define void @test(ptr %dst, ptr %src) {
  %d0 = load i16, ptr %dst, align 2
  %s0 = load i16, ptr %src, align 2
  %q = getelementptr i8, ptr %src, i64 2
  %s1 = load i16, ptr %q, align 2
  store i16 %d0, ptr %dst, align 2
  %p1 = getelementptr i8, ptr %dst, i64 2
  store i16 %s0, ptr %p1, align 2
  ret void
}
