; CHECK: 1 correct transformations

; Generated code must satisfy the full RV64 signext contract.
define signext i1 @test(i1 noundef zeroext %value) {
  ret i1 %value
}
