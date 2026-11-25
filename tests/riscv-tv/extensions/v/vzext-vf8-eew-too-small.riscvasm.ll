; CHECK: ERROR: integer extension whose source EEW is below 8
; at SEW = 32, vzext.vf8 would read 4-bit elements
define void @test() {
  ret void
}
