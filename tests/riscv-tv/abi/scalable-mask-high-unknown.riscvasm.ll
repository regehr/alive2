; CHECK: 1 incorrect transformations
; only the low bits of a mask argument's register are specified. an
; nxv1i1 has vscale elements, which is VLMAX at e8, mf8; this subtracts the
; count of set bits there from the count over the whole register, so the
; result is 0 only if the high bits were assumed to be zero
define i64 @test(<vscale x 1 x i1> %m) {
  ret i64 0
}
