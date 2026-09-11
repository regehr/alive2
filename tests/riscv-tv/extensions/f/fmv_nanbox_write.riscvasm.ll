; Narrow FMV writes discard upper integer bits and NaN-box the FP result.
define void @fmv_nanbox_write(i64 noundef %bits, ptr noundef %out) {
  %word = and i64 %bits, 4294967295
  %boxed_word = or i64 %word, -4294967296
  store i64 %boxed_word, ptr %out, align 1
  %p1 = getelementptr i64, ptr %out, i64 1
  %half = and i64 %bits, 65535
  %boxed_half = or i64 %half, -65536
  store i64 %boxed_half, ptr %p1, align 1
  %p2 = getelementptr i64, ptr %out, i64 2
  store i64 %bits, ptr %p2, align 1
  ret void
}
