define i59 @f(ptr %0, i32 %z) {
  %2 = tail call signext i59 %0(i32 %z)
  ret i59 %2
}
