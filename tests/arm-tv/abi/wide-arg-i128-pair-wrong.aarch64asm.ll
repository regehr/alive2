; CHECK: 1 incorrect transformations

; Reading the low limb where the high one belongs must be caught.
define i64 @f(i64 %p, i128 %x) {
  %s = lshr i128 %x, 64
  %t = trunc i128 %s to i64
  ret i64 %t
}
