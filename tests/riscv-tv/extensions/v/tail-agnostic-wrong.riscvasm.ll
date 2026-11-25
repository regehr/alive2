; CHECK: 1 incorrect transformations
; with ta, the tail element may also become all ones
; (e8 here: with wider elements the alignment check makes this slow to refute)
define void @test(ptr %dst) {
  store i8 7, ptr %dst
  %p1 = getelementptr i8, ptr %dst, i64 1
  store i8 1, ptr %p1
  ret void
}
