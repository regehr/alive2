define i64 @test24(ptr %0, i32 %1) {
  %3 = getelementptr inbounds i64, ptr %0, i32 %1
  %4 = load i64, ptr %3, align 8
  ret i64 %4
}