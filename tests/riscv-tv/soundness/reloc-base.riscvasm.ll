; CHECK: 1 incorrect transformations
; XFAIL: Transformation seems to be correct!
;
; ADDI from x0 materializes only the signed low 12 bits of g, not its address.
@g = external global i64
define ptr @test() { ret ptr @g }
