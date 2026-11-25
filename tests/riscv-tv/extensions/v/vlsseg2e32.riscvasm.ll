; a strided segment load: two-word segments, 12 bytes apart
define void @test(ptr %f0, ptr %src, ptr %f1) {
  %x0 = load i32, ptr %src, align 4
  %p1 = getelementptr i8, ptr %src, i64 4
  %y0 = load i32, ptr %p1, align 4
  %p2 = getelementptr i8, ptr %src, i64 12
  %x1 = load i32, ptr %p2, align 4
  %p3 = getelementptr i8, ptr %src, i64 16
  %y1 = load i32, ptr %p3, align 4
  store i32 %x0, ptr %f0, align 4
  %q1 = getelementptr i8, ptr %f0, i64 4
  store i32 %x1, ptr %q1, align 4
  store i32 %y0, ptr %f1, align 4
  %r1 = getelementptr i8, ptr %f1, i64 4
  store i32 %y1, ptr %r1, align 4
  ret void
}
