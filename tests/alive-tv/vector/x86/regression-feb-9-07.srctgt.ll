define <8 x i32> @src() {
  %calltmp = call <8 x i32> @llvm.x86.avx2.phsub.d(<8 x i32> <i32 -1306389437, i32 -1699807924, i32 -1725376897, i32 -995818298, i32 759098485, i32 -1239181406, i32 1140698766, i32 -2138096067>, <8 x i32> <i32 17, i32 18, i32 9, i32 2, i32 25, i32 29, i32 31, i32 4>)
  ret <8 x i32> %calltmp
}

define <8 x i32> @tgt() {
  ret <8 x i32> <i32 393418487, i32 -729558599, i32 -1, i32 7, i32 1998279891, i32 -1016172463, i32 -4, i32 27>
}

declare <8 x i32> @llvm.x86.avx2.phsub.d(<8 x i32>, <8 x i32>)
