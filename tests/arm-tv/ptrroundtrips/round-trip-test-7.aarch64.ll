; TEST-ARGS: -terse -test-replace-ptrtoint
; one ptrtoint feeding several round trips, as a stack pointer does in
; lifted code; each round trip should collapse into a gep
define void @s(ptr %p, i64 %v) {
    %sp = ptrtoint ptr %p to i64
    %i1 = add i64 %sp, -16
    %p1 = inttoptr i64 %i1 to ptr
    store i64 %v, ptr %p1, align 8
    %i2 = add i64 %sp, -12
    %p2 = inttoptr i64 %i2 to ptr
    store i64 %v, ptr %p2, align 4
    ret void
}

; CHECK: getelementptr i8, ptr %p, i64 -16
; CHECK: getelementptr i8, ptr %p, i64 -12
; CHECK-NOT: inttoptr
; CHECK-NOT: ptrtoint
