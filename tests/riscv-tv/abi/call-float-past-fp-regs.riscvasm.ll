; CHECK: 1 correct transformations

; The ninth float has no FP argument register left, so LP64D passes it in
; a0, like an integer of the same width.
declare void @consume(float, float, float, float, float, float, float, float,
                      float)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(float 1.0, float 1.0, float 1.0, float 1.0, float 1.0,
                     float 1.0, float 1.0, float 1.0, float 2.0)
  ret void
}
