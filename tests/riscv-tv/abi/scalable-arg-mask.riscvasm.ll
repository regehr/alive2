; CHECK: ERROR: Unsupported function argument
; nxv128i1 is wider than any legal mask type, so LLVM splits it, which we
; don't model
define <vscale x 128 x i1> @test(<vscale x 128 x i1> %m) {
  ret <vscale x 128 x i1> %m
}
