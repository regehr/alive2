; CHECK: 1 correct transformations

; A 128-bit value passed to, and returned from, a call.
declare i128 @g(i128)
define i64 @f(i128 %x) {
  %v = call i128 @g(i128 %x)
  %t = trunc i128 %v to i64
  ret i64 %t
}
