define i9 @f(ptr %0, i32 %z) {
  %2 = tail call signext i9 %0(i32 %z)
  ret i9 %2
}
