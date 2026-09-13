; CHECK: 1 incorrect transformations

; Check SP writes through register copies, not just immediate adjustments.
define i64 @test() {
  ret i64 42
}
