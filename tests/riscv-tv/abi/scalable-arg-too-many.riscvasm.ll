; CHECK: ERROR: Unsupported function argument
; a third m8 argument doesn't fit in v8-v23, so it would be passed by
; reference, which we don't model
define <vscale x 8 x i64> @test(<vscale x 8 x i64> %a, <vscale x 8 x i64> %b, <vscale x 8 x i64> %c) {
  ret <vscale x 8 x i64> %c
}
