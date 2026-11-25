; CHECK: ERROR: destination group overlaps a source group with a different EEW
; the wide source is v8-v9; a narrower destination may overlap it only at
; v8
define void @test() {
  ret void
}
