; clmulr(x, y)
; = clmul(zext(x, 2 * BW), zext(y, 2 * BW)) >> (BW - 1)
; = bitreverse(clmul(bitreverse(x)))

define i8 @src(i8 %x, i8 %y) {
  %extx = zext i8 %x to i16
  %exty = zext i8 %y to i16
  %clmul = call i16 @llvm.clmul.i16(i16 %extx, i16 %exty)
  %clmulr = lshr i16 %clmul, 7
  %trunc = trunc i16 %clmulr to i8
  ret i8 %trunc
}

define i8 @tgt(i8 %x, i8 %y) {
  %rx = call i8 @llvm.bitreverse.i8(i8 %x)
  %ry = call i8 @llvm.bitreverse.i8(i8 %y)
  %clmul = call i8 @llvm.clmul.i8(i8 %rx, i8 %ry)
  %clmulr = call i8 @llvm.bitreverse.i8(i8 %clmul)
  ret i8 %clmulr
}
