@ga = external global [1024 x i8], align 8

; Function Attrs: nounwind
define void @test11(i32 %0, i8 signext %1) {
  %3 = shl nsw i32 %0, 1
  %4 = getelementptr inbounds [1024 x i8], ptr @ga, i32 0, i32 %3
  store i8 %1, ptr %4, align 1
  ret void
}