define void @test23(ptr %0, i32 %1) {
  %3 = getelementptr inbounds i32, ptr %0, i32 %1
  store i32 1, ptr %3, align 4
  ret void
}