; TEST-ARGS: --max-vscale=2
; CHECK: Checking vscale = 2
; CHECK: byval(32)
; ERROR: Parameter attributes not refined
; CHECK-NOT: Transformation seems to be correct!

; Concrete attribute sizes must be reflected in printed IR and cache keys.
; Otherwise these bodies look identical at scale 2 and the syntactic check
; can incorrectly skip verification.
define i32 @src(ptr byval(<vscale x 4 x i32>) %p) vscale_range(2, 2) {
  %v = load i32, ptr %p
  ret i32 %v
}

define i32 @tgt(ptr byval([16 x i8]) align 16 %p) vscale_range(2, 2) {
  %v = load i32, ptr %p
  ret i32 %v
}
