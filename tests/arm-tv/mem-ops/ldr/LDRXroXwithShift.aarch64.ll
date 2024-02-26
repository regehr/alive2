define i64 @test23(ptr %0, i64 %1) {
  %3 = getelementptr inbounds i64, ptr %0, i64 %1
  %4 = load i64, ptr %3, align 4
  ret i64 %4
}