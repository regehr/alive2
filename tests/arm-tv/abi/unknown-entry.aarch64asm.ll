; CHECK: 1 incorrect transformations

; An untouched GPR is arbitrary, not zero.
define i64 @test() memory(none) speculatable {
  ret i64 0
}
