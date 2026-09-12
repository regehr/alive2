; CHECK: 1 incorrect transformations

; An explicit LLVM zeroext i32 contract requires a zero upper word.
define zeroext i32 @test() {
  ret i32 -1
}
