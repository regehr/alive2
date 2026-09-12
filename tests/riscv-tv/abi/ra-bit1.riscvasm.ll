; CHECK: 1 incorrect transformations
; CHECK-NOT: Transformation seems to be correct!

; Only bit 0 is ignored: changing bit 1 changes the return destination.
define void @test() {
  ret void
}
