define i39 @f(ptr %0, i32 %z) {
  %2 = tail call signext i39 %0(i32 %z)
  ret i39 %2
}
