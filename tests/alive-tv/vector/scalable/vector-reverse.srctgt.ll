; TEST-ARGS: --single-vscale=2

define <vscale x 2 x i32> @src(<vscale x 2 x i32> %a) {
  %r = call <vscale x 2 x i32> @llvm.vector.reverse.nxv2i32(<vscale x 2 x i32> %a)
  %e = extractelement <vscale x 2 x i32> %r, i64 0
  %i = insertelement <vscale x 2 x i32> %a, i32 %e, i64 0
  ret <vscale x 2 x i32> %i
}

define <vscale x 2 x i32> @tgt(<vscale x 2 x i32> %a) {
  %e = extractelement <vscale x 2 x i32> %a, i64 3
  %i = insertelement <vscale x 2 x i32> %a, i32 %e, i64 0
  ret <vscale x 2 x i32> %i
}
