; CHECK: ERROR: masked vector instruction that also reads v0 as data
; v0 would be read both as data and as the mask, with different EEWs,
; which is reserved
define void @test() {
  ret void
}
