; a three-field segment store interleaves r, g, b into rgbrgb
define void @test(ptr %dst, ptr %r, ptr %g, ptr %b) {
  %r0 = load i8, ptr %r, align 1
  %g0 = load i8, ptr %g, align 1
  %b0 = load i8, ptr %b, align 1
  %r1p = getelementptr i8, ptr %r, i64 1
  %g1p = getelementptr i8, ptr %g, i64 1
  %b1p = getelementptr i8, ptr %b, i64 1
  %r1 = load i8, ptr %r1p, align 1
  %g1 = load i8, ptr %g1p, align 1
  %b1 = load i8, ptr %b1p, align 1
  store i8 %r0, ptr %dst, align 1
  %d1 = getelementptr i8, ptr %dst, i64 1
  store i8 %g0, ptr %d1, align 1
  %d2 = getelementptr i8, ptr %dst, i64 2
  store i8 %b0, ptr %d2, align 1
  %d3 = getelementptr i8, ptr %dst, i64 3
  store i8 %r1, ptr %d3, align 1
  %d4 = getelementptr i8, ptr %dst, i64 4
  store i8 %g1, ptr %d4, align 1
  %d5 = getelementptr i8, ptr %dst, i64 5
  store i8 %b1, ptr %d5, align 1
  ret void
}
