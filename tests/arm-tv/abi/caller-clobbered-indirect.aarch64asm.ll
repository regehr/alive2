; CHECK: 1 incorrect transformations

; Indirect calls clobber the same argument registers as direct calls.
define i64 @test(ptr %callee) {
  %anchor = add i64 0, 0
  call void %callee()
  ret i64 42
}
