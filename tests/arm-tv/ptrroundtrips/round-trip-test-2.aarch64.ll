; should collapse into gep
define ptr @h(ptr %p, i64 %idx) {
    %i1 = ptrtoint ptr %p to i64
    %i2 = add i64 %idx, %i1
    %r = inttoptr i64 %i2 to ptr
    ret ptr %r
}
