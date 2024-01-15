@ga = external global [1024 x i8], align 8

; Function Attrs: nounwind memory(read)
define signext i8 @f(i32 %0) {
  %2 = getelementptr inbounds [1024 x i8], ptr @ga, i32 0, i32 %0
  %3 = load i8, ptr %2, align 1
  ret i8 %3
}