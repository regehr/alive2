; CHECK: 1 correct transformations

; Even signext i32 leaves bits 63:32 unspecified on AArch64.
define signext i32 @test() {
  ret i32 -1
}
