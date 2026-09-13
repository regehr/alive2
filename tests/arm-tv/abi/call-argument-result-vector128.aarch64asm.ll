; CHECK: 1 correct transformations

; Read arguments before clobbering and install the result afterwards.
declare <2 x i64> @callee(<2 x i64>)
define <2 x i64> @test(<2 x i64> %arg) {
  %anchor = add i64 0, 0
  %result = call <2 x i64> @callee(<2 x i64> %arg)
  ret <2 x i64> %result
}
