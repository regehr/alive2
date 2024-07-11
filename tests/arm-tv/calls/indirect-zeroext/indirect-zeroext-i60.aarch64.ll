define i60 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i60 %0(i32 %z)
  ret i60 %2
}
