define i63 @f(ptr %0, i32 %z) {
  %2 = tail call signext i63 %0(i32 %z)
  ret i63 %2
}
