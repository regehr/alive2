; CHECK: ERROR: Unsupported function argument
; with v8-v23 taken by two m8 arguments, c would be passed by reference,
; which we don't model
define i64 @test(<16 x i64> %a, <16 x i64> %b, <2 x i64> %c) {
  %e = extractelement <2 x i64> %c, i64 1
  ret i64 %e
}
