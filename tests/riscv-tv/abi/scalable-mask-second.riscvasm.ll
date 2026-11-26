; only the first mask goes in v0; the second is placed like LMUL = 1 data,
; in the first free register after d's v8
define <vscale x 2 x i1> @test(<vscale x 2 x i32> %d, <vscale x 2 x i1> %m1, <vscale x 2 x i1> %m2) {
  %r = and <vscale x 2 x i1> %m1, %m2
  ret <vscale x 2 x i1> %r
}
