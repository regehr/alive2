; CHECK: 1 incorrect transformations
; XFAIL: Transformation seems to be correct!
;
; The real eight-byte load combines a 32-bit relocation and the word 1.
; The lifter expands the 32-bit relocation to a full pointer and returns g.
@g = external global i8
define ptr @test() { ret ptr @g }
