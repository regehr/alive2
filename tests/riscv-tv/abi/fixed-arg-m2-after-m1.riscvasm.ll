; register groups are sized as if VLEN were 128, so a 256-bit vector
; needs an m2 group, which after a 128-bit argument in v8 is v10-v11
define <8 x i32> @test(<4 x i32> %x, <8 x i32> %a) {
  %r = add <8 x i32> %a, %a
  ret <8 x i32> %r
}
