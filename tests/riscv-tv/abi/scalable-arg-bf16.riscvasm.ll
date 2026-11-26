; RVA23U64 has no vector bfloat (Zvfbfmin)
; CHECK: ERROR: only vectors of integers, half, float, and double are supported
define <vscale x 4 x bfloat> @test(<vscale x 4 x bfloat> %a) {
  ret <vscale x 4 x bfloat> %a
}
