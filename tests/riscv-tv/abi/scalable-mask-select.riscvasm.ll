; a mask in v0 doesn't use up v8: the data arguments are in v8-v9 and
; v10-v11
define <vscale x 2 x i64> @test(<vscale x 2 x i1> %m, <vscale x 2 x i64> %a, <vscale x 2 x i64> %b) {
  %r = select <vscale x 2 x i1> %m, <vscale x 2 x i64> %a, <vscale x 2 x i64> %b
  ret <vscale x 2 x i64> %r
}
