; CHECK: 1 incorrect transformations
; XFAIL: Transformation seems to be correct!
;
; For p=null the source returns 42, but the target load must fault, even with rd=x0.
; Default O3 erases the load; -optimize-tgt=sroa correctly rejects this case.
define i64 @test(ptr %p) { ret i64 42 }
