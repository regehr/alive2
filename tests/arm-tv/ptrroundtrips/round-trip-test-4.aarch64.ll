; should not collapse gep
define ptr @c(ptr %p, i64 %idx) {
    %i1 = ptrtoint ptr %p to i64 ; can't destroy, has a use
    %i2 = add i64 %i1, %idx
    %r = inttoptr i64 %i2 to ptr

    %i3 = add i64 %i1, 1

    ret ptr %r
}
