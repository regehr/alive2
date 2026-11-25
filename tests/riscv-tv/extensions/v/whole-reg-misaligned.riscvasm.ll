; CHECK: 1 incorrect transformations
; vl1re32.v may fault unless the address is 4-byte aligned
define void @test(ptr %dst, ptr %src) {
  %lo = load i64, ptr %src, align 1
  %p1 = getelementptr i8, ptr %src, i64 8
  %hi = load i64, ptr %p1, align 1
  store i64 %lo, ptr %dst, align 1
  %q1 = getelementptr i8, ptr %dst, i64 8
  store i64 %hi, ptr %q1, align 1
  ret void
}
