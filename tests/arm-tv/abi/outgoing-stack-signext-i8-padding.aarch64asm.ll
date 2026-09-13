; CHECK: 1 correct transformations

; Outgoing stack arguments must extend to 32 bits; the upper word is padding.
declare void @consume(i64, i64, i64, i64, i64, i64, i64, i64, i8 signext)
define void @test(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e, i64 %f, i64 %g, i64 %h) {
  %anchor = add i64 0, 0
  call void @consume(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e, i64 %f, i64 %g, i64 %h, i8 -1)
  ret void
}
