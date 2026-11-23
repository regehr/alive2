; TEST-ARGS: --disable-undef-input
; left(a, b, 0) = a, left(a, b, N) = b, right(a, b, 0) = b, right(a, b, N) = a

define {<4 x i16>, <4 x i16>, <4 x i16>, <4 x i16>} @src(<4 x i16> %a, <4 x i16> %b) {
  %l0 = call <4 x i16> @llvm.vector.splice.left.v4i16(<4 x i16> %a, <4 x i16> %b, i32 0)
  %l4 = call <4 x i16> @llvm.vector.splice.left.v4i16(<4 x i16> %a, <4 x i16> %b, i32 4)
  %r0 = call <4 x i16> @llvm.vector.splice.right.v4i16(<4 x i16> %a, <4 x i16> %b, i32 0)
  %r4 = call <4 x i16> @llvm.vector.splice.right.v4i16(<4 x i16> %a, <4 x i16> %b, i32 4)
  %t0 = insertvalue {<4 x i16>, <4 x i16>, <4 x i16>, <4 x i16>} poison, <4 x i16> %l0, 0
  %t1 = insertvalue {<4 x i16>, <4 x i16>, <4 x i16>, <4 x i16>} %t0, <4 x i16> %l4, 1
  %t2 = insertvalue {<4 x i16>, <4 x i16>, <4 x i16>, <4 x i16>} %t1, <4 x i16> %r0, 2
  %t3 = insertvalue {<4 x i16>, <4 x i16>, <4 x i16>, <4 x i16>} %t2, <4 x i16> %r4, 3
  ret {<4 x i16>, <4 x i16>, <4 x i16>, <4 x i16>} %t3
}

define {<4 x i16>, <4 x i16>, <4 x i16>, <4 x i16>} @tgt(<4 x i16> %a, <4 x i16> %b) {
  %t0 = insertvalue {<4 x i16>, <4 x i16>, <4 x i16>, <4 x i16>} poison, <4 x i16> %a, 0
  %t1 = insertvalue {<4 x i16>, <4 x i16>, <4 x i16>, <4 x i16>} %t0, <4 x i16> %b, 1
  %t2 = insertvalue {<4 x i16>, <4 x i16>, <4 x i16>, <4 x i16>} %t1, <4 x i16> %b, 2
  %t3 = insertvalue {<4 x i16>, <4 x i16>, <4 x i16>, <4 x i16>} %t2, <4 x i16> %a, 3
  ret {<4 x i16>, <4 x i16>, <4 x i16>, <4 x i16>} %t3
}
