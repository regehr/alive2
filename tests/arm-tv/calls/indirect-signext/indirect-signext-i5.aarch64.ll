define i5 @f(ptr %0, i32 %z) {
  %2 = tail call signext i5 %0(i32 %z)
  ret i5 %2
}
