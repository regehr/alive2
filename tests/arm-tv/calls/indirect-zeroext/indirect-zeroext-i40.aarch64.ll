define i40 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i40 %0(i32 %z)
  ret i40 %2
}
