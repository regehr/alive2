; fixed-length masks are placed like scalable ones: the first in v0, the
; next like LMUL = 1 data, and a result in v0
define <4 x i1> @test(<4 x i1> %a, <4 x i1> %b) {
  %r = and <4 x i1> %a, %b
  ret <4 x i1> %r
}
