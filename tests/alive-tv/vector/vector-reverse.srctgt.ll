define <4 x i32> @src(<4 x i32> %a) {
  %r = call <4 x i32> @llvm.vector.reverse.v4i32(<4 x i32> %a)
  ret <4 x i32> %r
}

define <4 x i32> @tgt(<4 x i32> %a) {
  %r = shufflevector <4 x i32> %a, <4 x i32> poison, <4 x i32> <i32 3, i32 2, i32 1, i32 0>
  ret <4 x i32> %r
}
