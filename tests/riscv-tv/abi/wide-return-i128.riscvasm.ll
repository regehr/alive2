; CHECK: 1 correct transformations

; A 128-bit result comes back in a0 and a1, least significant first.
define i128 @f(i128 %x) {
  %r = or i128 %x, 1
  ret i128 %r
}
