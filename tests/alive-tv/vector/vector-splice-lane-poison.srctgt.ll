; only the lanes that come from the poison operand are poison
define i32 @src(<4 x i32> %a) {
  %r = call <4 x i32> @llvm.vector.splice.left.v4i32(<4 x i32> %a, <4 x i32> poison, i32 2)
  %e = extractelement <4 x i32> %r, i64 1
  ret i32 %e
}

define i32 @tgt(<4 x i32> %a) {
  %e = extractelement <4 x i32> %a, i64 3
  ret i32 %e
}
