; CHECK: 1 incorrect transformations
; XFAIL: Transformation seems to be correct!
;
; The .quad starts at byte 1, without implicit alignment. The lifter inserts
; seven padding bytes, moving the pointer to byte 8. The real load at p+8
; reads the pointer high byte and seven zero bytes, not the pointer.
@g = external global i8
define ptr @test() { ret ptr @g }
