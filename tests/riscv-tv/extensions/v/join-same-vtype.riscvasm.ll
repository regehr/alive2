; both paths set the same vtype, so it is known after the join
define void @test(ptr %dst, i64 %c) {
  store i64 0, ptr %dst, align 4
  %p1 = getelementptr i8, ptr %dst, i64 8
  store i64 0, ptr %p1, align 4
  ret void
}
