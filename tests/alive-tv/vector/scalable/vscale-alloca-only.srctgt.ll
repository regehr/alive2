; SKIP-IDENTITY
; CHECK: Checking vscale = 1
; CHECK-NOT: Transformation seems to be correct!
; ERROR: Could not translate 'src' to Alive IR

; Allocas carry the allocated type separately from their scalar result type.
; Scalable allocas are currently unsupported, but must trigger enumeration.
define ptr @src() {
  %p = alloca <vscale x 2 x i32>
  ret ptr %p
}
define ptr @tgt() {
  ret ptr null
}
