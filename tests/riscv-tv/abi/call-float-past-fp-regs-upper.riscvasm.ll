; CHECK: 1 correct transformations

; A float passed in a GPR has no NaN-boxing: the bits of a0 above the
; float are unspecified, so the caller may leave anything there.
declare void @consume(float, float, float, float, float, float, float, float,
                      float)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(float 1.0, float 1.0, float 1.0, float 1.0, float 1.0,
                     float 1.0, float 1.0, float 1.0, float 2.0)
  ret void
}
