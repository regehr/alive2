; CHECK: 1 correct transformations

; Explicit zeroext i32 differs from the usual C signext i32 contract.
define zeroext i32 @test() {
  ret i32 -1
}
