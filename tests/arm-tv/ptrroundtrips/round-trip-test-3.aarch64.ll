; should not collapse gep
define ptr @b(ptr %p, i64 %idx, ptr nocapture noundef %x) {
    %i1 = ptrtoint ptr %p to i64
    %i2 = add i64 %i1, %idx      ; can't destroy, has a use
    %r = inttoptr i64 %i2 to ptr

    store i64 %i2, ptr %x, align 8

    ret ptr %r
}
