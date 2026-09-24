; TEST-ARGS: --max-vscale=4 --disable-undef-input
; CHECK: Checking vscale = 1
; CHECK: Checking vscale = 2
; CHECK: Checking vscale = 4
; CHECK: Transformation seems to be correct!
; CHECK-NOT: ERROR:

; Both vector shapes and the scalar intrinsic must use the same assignment.
define i32 @src(i32 %x) {
  %v = call i32 @llvm.vscale.i32()
  %n = mul i32 %v, 2
  %last = sub i32 %n, 1
  %insert = insertelement <vscale x 2 x i32> poison, i32 %x, i32 0
  %splat = shufflevector <vscale x 2 x i32> %insert, <vscale x 2 x i32> poison, <vscale x 2 x i32> zeroinitializer
  %r = extractelement <vscale x 2 x i32> %splat, i32 %last
  ret i32 %r
}

define i32 @tgt(i32 %x) {
  %v = call i32 @llvm.vscale.i32()
  %n = mul i32 %v, 4
  %last = sub i32 %n, 1
  %insert = insertelement <vscale x 4 x i32> zeroinitializer, i32 %x, i32 %last
  %r = extractelement <vscale x 4 x i32> %insert, i32 %last
  ret i32 %r
}
