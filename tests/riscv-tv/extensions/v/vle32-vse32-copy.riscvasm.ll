; a unit-stride load and store of four 32-bit elements copies 16 bytes
define void @test(ptr %dst, ptr %src) {
  %lo = load i64, ptr %src, align 4
  %p1 = getelementptr i8, ptr %src, i64 8
  %hi = load i64, ptr %p1, align 4
  store i64 %lo, ptr %dst, align 4
  %q1 = getelementptr i8, ptr %dst, i64 8
  store i64 %hi, ptr %q1, align 4
  ret void
}
