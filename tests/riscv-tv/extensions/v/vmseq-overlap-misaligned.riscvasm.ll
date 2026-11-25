; CHECK: ERROR: compare whose destination overlaps a source group
; a mask destination may overlap a wider source group only at its
; lowest-numbered register; v9 is the second register of v8-v9
define void @test() {
  ret void
}
