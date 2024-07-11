define i27 @f(ptr %0, i32 %z) {
  %2 = tail call signext i27 %0(i32 %z)
  ret i27 %2
}
