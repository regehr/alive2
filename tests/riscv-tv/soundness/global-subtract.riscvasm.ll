; CHECK: 1 incorrect transformations
; XFAIL: Transformation seems to be correct!
;
; The table holds g-8, but the lifter changes subtraction to addition.
@g = external global [32 x i8]
define ptr @test() { ret ptr getelementptr (i8, ptr @g, i64 8) }
