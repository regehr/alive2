define i44 @f(ptr %0, i32 %z) {
  %2 = tail call signext i44 %0(i32 %z)
  ret i44 %2
}
