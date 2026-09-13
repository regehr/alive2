; CHECK: 1 incorrect transformations
; XFAIL: Transformation seems to be correct!
;
; LUI must overwrite a0 with an address high part, whose low 12 bits are zero.
; It cannot preserve the constant 42.
@g = external global i64
define i64 @test() { ret i64 42 }
