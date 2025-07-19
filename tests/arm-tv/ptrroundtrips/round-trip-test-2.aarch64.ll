; TEST-ARGS: -test-replace-ptrtoint
; should collapse into gep
define ptr @h(ptr %p, i64 %idx) {
    %i1 = ptrtoint ptr %p to i64
    %i2 = add i64 %idx, %i1     ; ptr is left operand here
    %r = inttoptr i64 %i2 to ptr
    ret ptr %r
}

; CHECK: getelementptr
; CHECK-NOT: inttoptr
; CHECK-NOT: ptrtoint
