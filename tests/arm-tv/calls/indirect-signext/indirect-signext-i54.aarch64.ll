define i54 @f(ptr %0, i32 %z) {
  %2 = tail call signext i54 %0(i32 %z)
  ret i54 %2
}
