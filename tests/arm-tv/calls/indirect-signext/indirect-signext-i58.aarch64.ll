define i58 @f(ptr %0, i32 %z) {
  %2 = tail call signext i58 %0(i32 %z)
  ret i58 %2
}
