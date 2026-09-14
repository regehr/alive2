; CHECK: 1 correct transformations

; The outgoing side of the same split, read back out of the caller's
; argument area at SP.
declare i64 @g(i64,i64,i64,i64,i64,i64,i64, i128)
define i64 @f(i64 %a, i128 %w) {
  %r = call i64 @g(i64 %a, i64 %a, i64 %a, i64 %a, i64 %a, i64 %a, i64 %a, i128 %w)
  ret i64 %r
}
