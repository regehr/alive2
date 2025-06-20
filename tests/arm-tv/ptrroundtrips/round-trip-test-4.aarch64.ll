; TEST-ARGS: -run-replace-ptrtoint
; should not collapse gep
define ptr @c(ptr %p, i64 %idx, ptr nocapture noundef %x) {
    %i1 = ptrtoint ptr %p to i64 ; can't destroy, has a use
    %i2 = add i64 %i1, %idx
    %r = inttoptr i64 %i2 to ptr

    store i64 %i1, ptr %x, align 8

    ret ptr %r
}

; CHECK-NOT: gep ptr
