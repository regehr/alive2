define i41 @f(ptr %0, i32 %z) {
  %2 = tail call signext i41 %0(i32 %z)
  ret i41 %2
}
