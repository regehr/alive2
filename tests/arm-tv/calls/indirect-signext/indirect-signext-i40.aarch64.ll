define i40 @f(ptr %0, i32 %z) {
  %2 = tail call signext i40 %0(i32 %z)
  ret i40 %2
}
