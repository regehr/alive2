; should collapse into gep
define ptr @f(ptr %p, i64 %idx) {
    %i1 = ptrtoint ptr %p to i64
    %i2 = add i64 %i1, %idx
    %r = inttoptr i64 %i2 to ptr
    ret ptr %r
}
