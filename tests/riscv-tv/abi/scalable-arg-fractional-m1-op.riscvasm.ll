; an m1 operation on mf2 values computes junk in the high part of the
; register, which is fine: only the low part is returned
define <vscale x 1 x i32> @test(<vscale x 1 x i32> %a, <vscale x 1 x i32> %b) {
  %r = xor <vscale x 1 x i32> %a, %b
  ret <vscale x 1 x i32> %r
}
