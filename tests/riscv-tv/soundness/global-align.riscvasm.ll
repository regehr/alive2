; CHECK: 1 incorrect transformations
; XFAIL: Transformation seems to be correct!
;
; The byte at p+1 is alignment padding (zero), not the later byte 2.
define i64 @test() { ret i64 2 }
