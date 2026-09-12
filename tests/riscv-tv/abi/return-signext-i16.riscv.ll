; CHECK: 1 correct transformations

; Generated code must satisfy the full RV64 signext contract.
define signext i16 @test(i16 noundef zeroext %value) {
  ret i16 %value
}
