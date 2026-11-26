; two m8 arguments fill v8-v15 and v16-v23
define <vscale x 8 x i64> @test(<vscale x 8 x i64> %a, <vscale x 8 x i64> %b) {
  %r = and <vscale x 8 x i64> %a, %b
  ret <vscale x 8 x i64> %r
}
