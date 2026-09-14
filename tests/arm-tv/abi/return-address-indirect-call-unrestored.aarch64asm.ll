; CHECK: 1 incorrect transformations

; BLR also overwrites LR.
define i64 @test(ptr %callee) {
  %anchor = add i64 0, 0
  %result = call i64 %callee()
  ret i64 %result
}
