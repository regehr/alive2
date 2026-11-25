; vid.v writes each element's index
define void @test(ptr %dst) {
  store i16 0, ptr %dst, align 2
  %p1 = getelementptr i8, ptr %dst, i64 2
  store i16 1, ptr %p1, align 2
  %p2 = getelementptr i8, ptr %dst, i64 4
  store i16 2, ptr %p2, align 2
  ret void
}
