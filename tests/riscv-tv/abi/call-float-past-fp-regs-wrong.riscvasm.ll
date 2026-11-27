; CHECK: 1 incorrect transformations

; The ninth float travels in a0, but the caller leaves 1.0 there, not 2.0.
declare void @consume(float, float, float, float, float, float, float, float,
                      float)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(float 1.0, float 1.0, float 1.0, float 1.0, float 1.0,
                     float 1.0, float 1.0, float 1.0, float 2.0)
  ret void
}
