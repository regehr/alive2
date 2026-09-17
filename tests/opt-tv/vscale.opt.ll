; TEST-ARGS: -passes=instcombine -tv-max-vscale=4
; CHECK: Checking vscale = 1
; CHECK: Checking vscale = 2
; CHECK: Checking vscale = 4
; CHECK: Transformation seems to be correct!
; CHECK-NOT: ERROR:
; CHECK-NOT: Transformation doesn't verify!

define <vscale x 2 x i32> @vector(<vscale x 2 x i32> %x) {
  %r = add <vscale x 2 x i32> %x, zeroinitializer
  ret <vscale x 2 x i32> %r
}

; Checking this function after a scalable function must still work with the
; shared type cache, including after the dependency is optimized away.
define i32 @scalar() {
  %v = call i32 @llvm.vscale.i32()
  %r = sub i32 %v, %v
  ret i32 %r
}

define <vscale x 2 x i32> @another_vector(<vscale x 2 x i32> %x) {
  %r = mul <vscale x 2 x i32> %x, zeroinitializer
  ret <vscale x 2 x i32> %r
}
