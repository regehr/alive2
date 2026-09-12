; CHECK: 1 incorrect transformations

; Both paths return the right value, but one fails to restore the stack pointer.
define i64 @sp_unrestored(i64 noundef %x) {
  ret i64 42
}
