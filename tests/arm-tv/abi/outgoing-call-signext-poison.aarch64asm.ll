; CHECK: 1 correct transformations

; A poison argument does not require a valid sign-extended representation.
declare void @consume(i8)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i8 signext poison)
  ret void
}
