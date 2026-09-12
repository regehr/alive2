; CHECK: 1 incorrect transformations

; The low bits are correct, but bit 63 of the NaN box is clear.
define float @nanbox_return_float_wrong() {
  ret float 1.0
}
