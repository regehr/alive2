; TEST-ARGS: --max-vscale=2
; CHECK: Checking vscale = 2
; CHECK: Transformation seems to be correct!
; CHECK-NOT: ERROR:

; The byval call argument's size must be fixed up, even though the call and
; its operand have scalar types.
declare void @consume(ptr)

define void @src(ptr %p) vscale_range(2, 2) {
  call void @consume(ptr byval(<vscale x 4 x i32>) %p)
  ret void
}

define void @tgt(ptr %p) vscale_range(2, 2) {
  call void @consume(ptr byval([32 x i8]) align 16 %p)
  ret void
}
