define i51 @f(ptr %0, i32 %z) {
  %2 = tail call zeroext i51 %0(i32 %z)
  ret i51 %2
}
