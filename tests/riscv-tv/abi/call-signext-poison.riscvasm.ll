; CHECK: 1 correct transformations

; Passing poison does not require a canonical register representation.
declare void @consume(i8 signext)
define void @test() {
  %anchor = add i64 0, 0
  call void @consume(i8 signext poison)
  ret void
}
