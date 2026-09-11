; FNMADD negates the product before adding, so exact cancellation under RNE is +0.
define i64 @fnmadd_zero() {
  ret i64 0
}
