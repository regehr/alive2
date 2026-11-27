; llc swaps the two floats with an e64 vror.vi. a lane that violates
; nofpclass is poison in the source, but must not poison the other half of
; the 64-bit element in the lifted code
define <2 x float> @f(<2 x float> nofpclass(inf) %a) {
  %r = shufflevector <2 x float> %a, <2 x float> poison, <2 x i32> <i32 1, i32 0>
  ret <2 x float> %r
}
