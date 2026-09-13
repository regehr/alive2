; CHECK: 1 incorrect transformations

; Even an unused, immediately restored misaligned SP violates the ABI.
define i64 @test() {
  ret i64 42
}
