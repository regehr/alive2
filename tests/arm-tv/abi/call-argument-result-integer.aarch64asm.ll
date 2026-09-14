; CHECK: 1 correct transformations

; Read arguments before clobbering and install the result afterwards.
declare i64 @callee(i64)
define i64 @test(i64 %arg) {
  %anchor = add i64 0, 0
  %result = call i64 @callee(i64 %arg)
  ret i64 %result
}
