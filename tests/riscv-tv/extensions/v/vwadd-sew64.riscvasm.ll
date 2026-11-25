; CHECK: ERROR: widening or narrowing instruction with SEW = 64
; the result would have 128-bit elements
define void @test() {
  ret void
}
