; a negative stride stores the elements in reverse
define void @test(ptr %dst, ptr %src) {
  %a0 = load i32, ptr %src, align 4
  %p1 = getelementptr i8, ptr %src, i64 4
  %a1 = load i32, ptr %p1, align 4
  %q1 = getelementptr i8, ptr %dst, i64 4
  store i32 %a0, ptr %q1, align 4
  store i32 %a1, ptr %dst, align 4
  ret void
}
