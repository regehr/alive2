define i35 @f(ptr %0, i32 %z) {
  %2 = tail call signext i35 %0(i32 %z)
  ret i35 %2
}
