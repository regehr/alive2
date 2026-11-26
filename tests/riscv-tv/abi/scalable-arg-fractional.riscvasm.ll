; an mf2 type occupies the low half of one register; the rest of v8 and v9
; is unspecified on entry, and ignored on return
define <vscale x 1 x i32> @test(<vscale x 1 x i32> %a, <vscale x 1 x i32> %b) {
  %r = xor <vscale x 1 x i32> %a, %b
  ret <vscale x 1 x i32> %r
}
