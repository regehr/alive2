; with LMUL=2 and VLEN=128, the group v8-v9 holds four 64-bit elements
define void @test(ptr %dst, ptr %src) {
  %a = load i64, ptr %src, align 8
  %s1 = getelementptr i8, ptr %src, i64 8
  %b = load i64, ptr %s1, align 8
  %s2 = getelementptr i8, ptr %src, i64 16
  %c = load i64, ptr %s2, align 8
  %s3 = getelementptr i8, ptr %src, i64 24
  %d = load i64, ptr %s3, align 8
  store i64 %a, ptr %dst, align 8
  %d1 = getelementptr i8, ptr %dst, i64 8
  store i64 %b, ptr %d1, align 8
  %d2 = getelementptr i8, ptr %dst, i64 16
  store i64 %c, ptr %d2, align 8
  %d3 = getelementptr i8, ptr %dst, i64 24
  store i64 %d, ptr %d3, align 8
  ret void
}
