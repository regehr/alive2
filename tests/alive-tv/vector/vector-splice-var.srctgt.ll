; TEST-ARGS: --disable-undef-input
; left(a, b, k) == right(a, b, N - k), including when k > N (both poison)
define <4 x float> @src(<4 x float> %a, <4 x float> %b, i32 %k) {
  %r = call <4 x float> @llvm.vector.splice.left.v4f32(<4 x float> %a, <4 x float> %b, i32 %k)
  ret <4 x float> %r
}

define <4 x float> @tgt(<4 x float> %a, <4 x float> %b, i32 %k) {
  %n = sub i32 4, %k
  %r = call <4 x float> @llvm.vector.splice.right.v4f32(<4 x float> %a, <4 x float> %b, i32 %n)
  ret <4 x float> %r
}
