define <16 x i16> @src() {
  %calltmp = call <16 x i16> @llvm.x86.avx2.phadd.w(<16 x i16> <i16 -23974, i16 -14890, i16 -25912, i16 30689, i16 -5316, i16 12682, i16 25468, i16 30283, i16 19045, i16 2503, i16 -13296, i16 26823, i16 -6801, i16 3895, i16 -15459, i16 -29275>, <16 x i16> <i16 39, i16 35, i16 15, i16 26, i16 11, i16 35, i16 6, i16 12, i16 22, i16 29, i16 32, i16 30, i16 17, i16 2, i16 21, i16 35>)
  ret <16 x i16> %calltmp
}

define <16 x i16> @tgt() {
  ret <16 x i16> <i16 26672, i16 4777, i16 7366, i16 -9785, i16 74, i16 41, i16 46, i16 18, i16 21548, i16 13527, i16 -2906, i16 20802, i16 51, i16 62, i16 19, i16 56>
}

declare <16 x i16> @llvm.x86.avx2.phadd.w(<16 x i16>, <16 x i16>)
