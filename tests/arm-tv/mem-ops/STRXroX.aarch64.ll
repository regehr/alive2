define void @test11(ptr %0) {
  %2 = getelementptr inbounds i64, ptr %0, i64 4096
  store i64 1, ptr %2, align 4
  ret void
}