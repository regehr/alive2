define i64 @test11(ptr %0) {
  %2 = getelementptr inbounds i64, ptr %0, i64 4096
  %3 = load i64, ptr %2, align 4
  ret i64 %3
}