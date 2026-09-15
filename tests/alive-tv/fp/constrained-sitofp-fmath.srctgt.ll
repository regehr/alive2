; Fast-math flags reach FpConversionOp through the constrained intrinsics too,
; and have to compose with a non-default rounding mode.

define double @src(i32 %x) {
  %r = call nsz double @llvm.experimental.constrained.sitofp.f64.i32(i32 %x, metadata !"round.towardzero", metadata !"fpexcept.ignore")
  ret double %r
}

define double @tgt(i32 %x) {
  %r = call double @llvm.experimental.constrained.sitofp.f64.i32(i32 %x, metadata !"round.towardzero", metadata !"fpexcept.ignore")
  ret double %r
}

declare double @llvm.experimental.constrained.sitofp.f64.i32(i32, metadata, metadata)
