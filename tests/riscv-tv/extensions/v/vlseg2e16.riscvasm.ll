; a two-field segment load deinterleaves (re, im) pairs into v8 and v9
define void @test(ptr %re, ptr %src, ptr %im) {
  %x0 = load i16, ptr %src, align 2
  %p1 = getelementptr i8, ptr %src, i64 2
  %x1 = load i16, ptr %p1, align 2
  %p2 = getelementptr i8, ptr %src, i64 4
  %x2 = load i16, ptr %p2, align 2
  %p3 = getelementptr i8, ptr %src, i64 6
  %x3 = load i16, ptr %p3, align 2
  store i16 %x0, ptr %re, align 2
  %r1 = getelementptr i8, ptr %re, i64 2
  store i16 %x2, ptr %r1, align 2
  store i16 %x1, ptr %im, align 2
  %i1 = getelementptr i8, ptr %im, i64 2
  store i16 %x3, ptr %i1, align 2
  ret void
}
