; TEST-ARGS: --max-vscale=4 --bidirectional
; SKIP-IDENTITY
; ERROR: Unsupported change to vscale_range
; CHECK: 0 correct transformations
; CHECK: 0 incorrect transformations
; CHECK: 7 Alive2 errors
; CHECK-NOT: Transformation seems to be correct!
; CHECK-NOT: Checking vscale =

; Reject both narrowing and widening, including changes outside the tested
; scales. (Functions that don't depend on vscale are exempt; see
; vscale-range-scalar.srctgt.ll.)
define <vscale x 2 x i8> @src_lower(<vscale x 2 x i8> %x) vscale_range(1,4) {
  ret <vscale x 2 x i8> %x
}
define <vscale x 2 x i8> @tgt_lower(<vscale x 2 x i8> %x) vscale_range(2,4) {
  ret <vscale x 2 x i8> %x
}

define <vscale x 2 x i8> @src_upper(<vscale x 2 x i8> %x) vscale_range(1,8) {
  ret <vscale x 2 x i8> %x
}
define <vscale x 2 x i8> @tgt_upper(<vscale x 2 x i8> %x) vscale_range(1,4) {
  ret <vscale x 2 x i8> %x
}

define <vscale x 2 x i8> @src_widen(<vscale x 2 x i8> %x) vscale_range(2,4) {
  ret <vscale x 2 x i8> %x
}
define <vscale x 2 x i8> @tgt_widen(<vscale x 2 x i8> %x) vscale_range(1,4) {
  ret <vscale x 2 x i8> %x
}

define <vscale x 2 x i8> @src_add(<vscale x 2 x i8> %x) {
  ret <vscale x 2 x i8> %x
}
define <vscale x 2 x i8> @tgt_add(<vscale x 2 x i8> %x) vscale_range(1,0) {
  ret <vscale x 2 x i8> %x
}

define <vscale x 2 x i8> @src_remove(<vscale x 2 x i8> %x) vscale_range(1,4) {
  ret <vscale x 2 x i8> %x
}
define <vscale x 2 x i8> @tgt_remove(<vscale x 2 x i8> %x) {
  ret <vscale x 2 x i8> %x
}

define <vscale x 2 x i8> @src_unbounded(<vscale x 2 x i8> %x) vscale_range(1,0) {
  ret <vscale x 2 x i8> %x
}
define <vscale x 2 x i8> @tgt_unbounded(<vscale x 2 x i8> %x) vscale_range(1,4) {
  ret <vscale x 2 x i8> %x
}

; Only one side needs to depend on vscale.
define i32 @src_one_side(i32 %x) vscale_range(1,4) {
  ret i32 %x
}
define i32 @tgt_one_side(i32 %x) vscale_range(2,4) {
  %v = call i32 @llvm.vscale.i32()
  %z = sub i32 %v, %v
  %r = add i32 %x, %z
  ret i32 %r
}
