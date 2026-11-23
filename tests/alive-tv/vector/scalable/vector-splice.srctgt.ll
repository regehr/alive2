; TEST-ARGS: --single-vscale=2

define <vscale x 2 x i32> @src(<vscale x 2 x i32> %a, <vscale x 2 x i32> %b, i32 %k) {
  %r = call <vscale x 2 x i32> @llvm.vector.splice.left.nxv2i32(<vscale x 2 x i32> %a, <vscale x 2 x i32> %b, i32 %k)
  ret <vscale x 2 x i32> %r
}

define <vscale x 2 x i32> @tgt(<vscale x 2 x i32> %a, <vscale x 2 x i32> %b, i32 %k) {
  %vs = call i32 @llvm.vscale.i32()
  %n = shl i32 %vs, 1
  %m = sub i32 %n, %k
  %r = call <vscale x 2 x i32> @llvm.vector.splice.right.nxv2i32(<vscale x 2 x i32> %a, <vscale x 2 x i32> %b, i32 %m)
  ret <vscale x 2 x i32> %r
}
