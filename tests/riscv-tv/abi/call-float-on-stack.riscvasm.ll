; CHECK: 1 correct transformations

; With both the FP and the integer argument registers used up, the ninth float
; goes on the stack. The caller writes only its four bytes of the slot.
declare void @consume(i64, i64, i64, i64, i64, i64, i64, i64,
                      float, float, float, float, float, float, float, float,
                      float)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0, i64 0,
                     float 1.0, float 1.0, float 1.0, float 1.0, float 1.0,
                     float 1.0, float 1.0, float 1.0, float 2.0)
  ret void
}
