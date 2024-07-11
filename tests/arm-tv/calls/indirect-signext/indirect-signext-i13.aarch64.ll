define i13 @f(ptr %0, i32 %z) {
  %2 = tail call signext i13 %0(i32 %z)
  ret i13 %2
}
