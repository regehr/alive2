; an indexed load (gather) adds each byte offset to the base
define void @test(ptr %dst, ptr %base, ptr %offs) {
  %o0 = load i32, ptr %offs, align 4
  %q1 = getelementptr i8, ptr %offs, i64 4
  %o1 = load i32, ptr %q1, align 4
  %w0 = zext i32 %o0 to i64
  %w1 = zext i32 %o1 to i64
  %p0 = getelementptr i8, ptr %base, i64 %w0
  %p1 = getelementptr i8, ptr %base, i64 %w1
  %a0 = load i32, ptr %p0, align 4
  %a1 = load i32, ptr %p1, align 4
  store i32 %a0, ptr %dst, align 4
  %d1 = getelementptr i8, ptr %dst, i64 4
  store i32 %a1, ptr %d1, align 4
  ret void
}
