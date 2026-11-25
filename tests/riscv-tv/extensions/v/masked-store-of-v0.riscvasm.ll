; CHECK: ERROR: masked vector memory access whose data group includes v0
; v0 would be read both as data and as the mask, with different EEWs,
; which is reserved. (the assembler already rejects the load form)
define void @test(ptr %dst) {
  ret void
}
