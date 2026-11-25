; CHECK: ERROR: segment access with EMUL * NFIELDS > 8
; three fields at EMUL = 4 would need twelve registers
define void @test(ptr %p) {
  ret void
}
