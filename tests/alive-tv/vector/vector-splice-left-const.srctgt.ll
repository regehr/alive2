define <4 x i32> @src(<4 x i32> %a, <4 x i32> %b) {
  %r = call <4 x i32> @llvm.vector.splice.left.v4i32(<4 x i32> %a, <4 x i32> %b, i32 1)
  ret <4 x i32> %r
}

define <4 x i32> @tgt(<4 x i32> %a, <4 x i32> %b) {
  %r = shufflevector <4 x i32> %a, <4 x i32> %b, <4 x i32> <i32 1, i32 2, i32 3, i32 4>
  ret <4 x i32> %r
}
