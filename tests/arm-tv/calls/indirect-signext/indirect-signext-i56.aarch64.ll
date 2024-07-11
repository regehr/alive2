define i56 @f(ptr %0, i32 %z) {
  %2 = tail call signext i56 %0(i32 %z)
  ret i56 %2
}
