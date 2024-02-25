define void @test23(ptr %0, i64 %1) {
  %3 = getelementptr inbounds i64, ptr %0, i64 %1
  store i64 1, ptr %3, align 4
  ret void
}