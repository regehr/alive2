; CHECK: 1 correct transformations

; Read arguments before clobbering and install the result afterwards.
declare double @callee(double)
define double @test(double %arg) {
  %anchor = add i64 0, 0
  %result = call double @callee(double %arg)
  ret double %result
}
