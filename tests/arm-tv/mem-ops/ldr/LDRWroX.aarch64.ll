define i32 @test11(ptr %0) {
  %2 = getelementptr inbounds i32, ptr %0, i32 4096
  %3 = load i32, ptr %2, align 4
  ret i32 %3
}