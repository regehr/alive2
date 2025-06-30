target triple = "riscv64"

define i1 @tf_0_foo(i32 %x) {
entry:
  ; (x >> 6) | (x << (32-6))
  %hi = lshr i32 %x, 6
  %lo = shl  i32 %x, 26        ; 32−6 = 26
  %r  = or   i32 %hi, %lo
  %cmp = icmp ugt i32 %r, 13371337
  ret  i1 %cmp
}
