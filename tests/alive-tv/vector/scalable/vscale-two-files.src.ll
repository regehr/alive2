; TEST-ARGS: --max-vscale=4
; CHECK: Checking vscale = 4
; CHECK: 1 correct transformations
; CHECK: 0 incorrect transformations
; CHECK-NOT: ERROR:

define i32 @f(i32 %x) {
  %v = call i32 @llvm.vscale.i32()
  %r = add i32 %x, %v
  ret i32 %r
}
