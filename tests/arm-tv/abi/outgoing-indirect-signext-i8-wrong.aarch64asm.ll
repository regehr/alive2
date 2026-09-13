; CHECK: 1 incorrect transformations

; Indirect-call arguments obey their call site's extension attributes.
define void @test(ptr %callee) {
  %anchor = add i64 0, 0
  call void %callee(i8 signext -1)
  ret void
}
