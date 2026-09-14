; CHECK: 1 incorrect transformations

; Reading the wrong stack slot for a limb must be caught.
define i64 @f(i64 %a,i64 %b,i64 %c,i64 %d,i64 %e,i64 %g,i64 %h, i128 %w) {
  %s = lshr i128 %w, 64
  %t = trunc i128 %s to i64
  ret i64 %t
}
