define i8 @src(i8 %val, i8 %mask) {
  %pext = call i8 @llvm.pext.i8(i8 %val, i8 %mask)
  %pdep = call i8 @llvm.pdep.i8(i8 %pext, i8 %mask)
  ret i8 %pdep
}

define i8 @tgt(i8 %val, i8 %mask) {
  %masked = and i8 %val, %mask
  ret i8 %masked
}
