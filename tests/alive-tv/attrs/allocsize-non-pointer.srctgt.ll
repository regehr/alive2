; LLVM only requires an allocsize argument to name an integer parameter, not
; the function to return a pointer, so this declaration is valid IR. Alive2
; models an allocation as a pointer to a fresh object and used to assert on
; the non-pointer result.

declare i64 @g0(i64, i64) #0

define i64 @src(i64 %x) {
  %r = call i64 @g0(i64 0, i64 %x)
  ret i64 %r
}

define i64 @tgt(i64 %x) {
  %r = call i64 @g0(i64 0, i64 %x)
  ret i64 %r
}

attributes #0 = { allocsize(1) }
