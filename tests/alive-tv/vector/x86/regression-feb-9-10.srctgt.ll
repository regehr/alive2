define <16 x i16> @src() {
  %calltmp = call <16 x i16> @llvm.x86.avx2.pmadd.ub.sw(<32 x i8> <i8 -82, i8 -58, i8 92, i8 -13, i8 -51, i8 -87, i8 -94, i8 -115, i8 -82, i8 -12, i8 -104, i8 -40, i8 -123, i8 -26, i8 122, i8 -24, i8 -123, i8 25, i8 23, i8 -4, i8 -10, i8 -112, i8 44, i8 -115, i8 -102, i8 120, i8 30, i8 -58, i8 -84, i8 59, i8 -109, i8 -96>, <32 x i8> <i8 31, i8 24, i8 9, i8 22, i8 14, i8 27, i8 23, i8 6, i8 24, i8 5, i8 28, i8 33, i8 18, i8 28, i8 39, i8 31, i8 3, i8 31, i8 26, i8 7, i8 0, i8 15, i8 23, i8 11, i8 36, i8 5, i8 9, i8 21, i8 6, i8 19, i8 35, i8 34>)
  ret <16 x i16> %calltmp
}

define <16 x i16> @tgt() {
  ret <16 x i16> <i16 10146, i16 6174, i16 7433, i16 4572, i16 5396, i16 11384, i16 8834, i16 11950, i16 1174, i16 2362, i16 2160, i16 2563, i16 6144, i16 4428, i16 2153, i16 10585>
}

declare  <16 x i16> @llvm.x86.avx2.pmadd.ub.sw(<32 x i8>, <32 x i8>)
