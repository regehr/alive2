; vmv2r.v copies a two-register group, ignoring vl
define void @test(ptr %dst, ptr %src) {
  %a = load i64, ptr %src, align 1
  %s1 = getelementptr i8, ptr %src, i64 8
  %b = load i64, ptr %s1, align 1
  %s2 = getelementptr i8, ptr %src, i64 16
  %c = load i64, ptr %s2, align 1
  %s3 = getelementptr i8, ptr %src, i64 24
  %d = load i64, ptr %s3, align 1
  store i64 %a, ptr %dst, align 1
  %d1 = getelementptr i8, ptr %dst, i64 8
  store i64 %b, ptr %d1, align 1
  %d2 = getelementptr i8, ptr %dst, i64 16
  store i64 %c, ptr %d2, align 1
  %d3 = getelementptr i8, ptr %dst, i64 24
  store i64 %d, ptr %d3, align 1
  ret void
}
