; TEST-ARGS: --max-vscale=4
; CHECK: Checking vscale = 4
; CHECK: Checking vscale = 1
; CHECK: 2 correct transformations
; CHECK: 0 incorrect transformations
; CHECK: 0 failed-to-prove transformations
; CHECK: 0 Alive2 errors

; The same scalable type is reused at different scales in successive pairs.
define void @src(ptr byval(<vscale x 4 x i32>) %p) vscale_range(4) {
  ret void
}
define void @tgt(ptr byval([64 x i8]) align 16 %p) vscale_range(4) {
  ret void
}

define void @src1(ptr byval(<vscale x 4 x i32>) %p) vscale_range(1) {
  ret void
}
define void @tgt1(ptr byval([16 x i8]) align 16 %p) vscale_range(1) {
  ret void
}
