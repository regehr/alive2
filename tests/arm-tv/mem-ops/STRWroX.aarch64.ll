define void @test11(ptr %0) {
  %2 = getelementptr inbounds i32, ptr %0, i32 4096
  store i32 1, ptr %2, align 4
  ret void
}