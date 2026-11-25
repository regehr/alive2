; vsetvli zero, zero keeps vl when the SEW/LMUL ratio, and so VLMAX, is
; unchanged
define i64 @test() {
  ret i64 3
}
