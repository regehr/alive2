; CHECK: ERROR: vector half-precision arithmetic needs Zvfh
define <2 x half> @test(<2 x half> %x, <2 x half> %y) {
  %r = fadd <2 x half> %x, %y
  ret <2 x half> %r
}
