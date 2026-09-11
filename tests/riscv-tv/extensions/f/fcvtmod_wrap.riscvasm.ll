; Adding/subtracting 2^32 and a fractional part must preserve the low integer word.
; The double-precision operations below are exact for every signed i32 input.
define void @fcvtmod_wrap(i32 noundef signext %x, ptr noundef %out) {
  %result = sext i32 %x to i64
  store i64 %result, ptr %out, align 1
  %p1 = getelementptr i64, ptr %out, i64 1
  store i64 %result, ptr %p1, align 1
  ret void
}
