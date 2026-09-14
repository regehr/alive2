; CHECK: 1 incorrect transformations

; Reading the wrong stack slot for a limb must be caught.
define i64 @f(i64 %a,i64 %b,i64 %c,i64 %d,i64 %e,i64 %g, i192 %w) {
  %s = lshr i192 %w, 128
  %t = trunc i192 %s to i64
  ret i64 %t
}
