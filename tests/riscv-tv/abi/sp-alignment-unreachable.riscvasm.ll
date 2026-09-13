; CHECK: 1 correct transformations

; A misaligned SP update on an unreachable path must not reject valid code.
define i64 @test() {
  ret i64 42
}
