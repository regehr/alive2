; CHECK: ERROR: Unsupported function argument
; a 2048-bit vector is split across two m8 groups, which we don't model
define <32 x i64> @test(<32 x i64> %a) {
  ret <32 x i64> %a
}
