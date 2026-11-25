; vwredsumu.vs zero-extends the SEW elements and sums into 2*SEW
define i64 @test(ptr %src) {
  %a0 = load i8, ptr %src, align 1
  %p1 = getelementptr i8, ptr %src, i64 1
  %a1 = load i8, ptr %p1, align 1
  %p2 = getelementptr i8, ptr %src, i64 2
  %a2 = load i8, ptr %p2, align 1
  %p3 = getelementptr i8, ptr %src, i64 3
  %a3 = load i8, ptr %p3, align 1
  %w0 = zext i8 %a0 to i16
  %w1 = zext i8 %a1 to i16
  %w2 = zext i8 %a2 to i16
  %w3 = zext i8 %a3 to i16
  %s01 = add i16 %w0, %w1
  %s23 = add i16 %w2, %w3
  %s = add i16 %s01, %s23
  %r = sext i16 %s to i64
  ret i64 %r
}
