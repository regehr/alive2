; CHECK: 1 incorrect transformations

; A call overwrites ra rather than preserving its previous value.
declare void @clobber()
define i64 @test() {
  %anchor = add i64 0, 0
  call void @clobber()
  ret i64 42
}
