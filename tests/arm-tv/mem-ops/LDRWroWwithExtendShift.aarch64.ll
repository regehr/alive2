define i32 @test23(ptr %0, i32 %1) {
  %3 = getelementptr inbounds i32, ptr %0, i32 %1
  %4 = load i32, ptr %3, align 4
  ret i32 %4
}