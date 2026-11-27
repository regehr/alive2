; CHECK: 1 incorrect transformations

; The ninth float belongs at 0(sp), but the caller stores it at 4(sp).
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
