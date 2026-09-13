; CHECK: 1 incorrect transformations
;
; SP is restored at return but is misaligned at the call, violating the ABI.
; A callee returning (sp & 15) returns 8 here and 0 with a conforming caller.
declare i64 @check_alignment()
define i64 @test() {
 %v = call i64 @check_alignment()
 ret i64 %v
}
