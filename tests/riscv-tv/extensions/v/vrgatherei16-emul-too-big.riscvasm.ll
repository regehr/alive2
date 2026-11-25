; CHECK: ERROR: vrgatherei16 whose index EMUL is out of range
; at SEW = 8 and LMUL = 8, the 16-bit indices would need EMUL = 16
define void @test() {
  ret void
}
