; CHECK: ERROR: vtype 221 is not a setting that RVA23U64 is required to support
; a fractional LMUL only has to support SEW up to LMUL * ELEN
define i64 @test() {
  ret i64 0
}
