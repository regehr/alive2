; offset > N gives poison
define <4 x i32> @src(<4 x i32> %a, <4 x i32> %b) {
  %r = call <4 x i32> @llvm.vector.splice.left.v4i32(<4 x i32> %a, <4 x i32> %b, i32 5)
  ret <4 x i32> %r
}

define <4 x i32> @tgt(<4 x i32> %a, <4 x i32> %b) {
  ret <4 x i32> poison
}
