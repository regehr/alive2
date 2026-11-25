; with tu, the tail elements 2 and 3 keep their old values
define void @test(ptr %dst) {
  store i32 7, ptr %dst, align 4
  %p1 = getelementptr i8, ptr %dst, i64 4
  store i32 7, ptr %p1, align 4
  %p2 = getelementptr i8, ptr %dst, i64 8
  store i32 1, ptr %p2, align 4
  %p3 = getelementptr i8, ptr %dst, i64 12
  store i32 1, ptr %p3, align 4
  ret void
}
