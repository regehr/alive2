; Function Attrs: nounwind
define void @f(ptr %0, i32 %1) {
  %3 = trunc i32 %1 to i8
  %4 = getelementptr inbounds i8, ptr %0, i32 -1
  store i8 %3, ptr %4, align 1
  ret void
}