; CHECK: 1 incorrect transformations

; An untouched register is arbitrary, not zero, even in a memory(none) function.
define i64 @test() memory(none) speculatable {
  ret i64 0
}
