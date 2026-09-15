; Alive2 only models fast-math flags on fpext and fptrunc, but LLVM allows them
; on sitofp and uitofp too, and llvm2alive used to hand them to FpConversionOp
; regardless -- which asserts. Refuse the instruction instead of aborting.

define double @src(i32 %x) {
  %r = sitofp nsz i32 %x to double
  ret double %r
}

define double @tgt(i32 %x) {
  %r = sitofp nsz i32 %x to double
  ret double %r
}

; ERROR: Unsupported instruction:

; SKIP-IDENTITY
