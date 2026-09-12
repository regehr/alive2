; CHECK: 1 incorrect transformations

; A void callee may overwrite floating-point argument registers.
declare void @clobber()
define double @test() {
  %anchor = add i64 0, 0
  call void @clobber()
  ret double 1.0
}
