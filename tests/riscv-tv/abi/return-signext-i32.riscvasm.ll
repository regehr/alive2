; CHECK: 1 correct transformations

; A valid signed word return extends bit 31 through the high word.
define signext i32 @test() {
  ret i32 -1
}
