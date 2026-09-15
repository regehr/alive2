; nnan is vacuous on an int -> fp conversion: an integer operand is never NaN
; and the result never is either, so the flag can be added freely.

define double @src(i32 %x) {
  %r = sitofp i32 %x to double
  ret double %r
}

define double @tgt(i32 %x) {
  %r = sitofp nnan i32 %x to double
  ret double %r
}
