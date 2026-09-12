; CHECK: 1 incorrect transformations
; CHECK-NOT: Transformation seems to be correct!

; ret jumps through ra; overwriting it cannot implement an ordinary return.
define void @test() {
  ret void
}
