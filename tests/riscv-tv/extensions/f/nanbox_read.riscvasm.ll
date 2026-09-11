; D: non-transfer operations treat improperly NaN-boxed narrow inputs as canonical NaNs.
define void @nanbox_read(ptr noundef %out) {
  ; Unboxed single-precision zero is classified as a quiet NaN.
  %p0 = getelementptr i64, ptr %out, i64 0
  store i64 512, ptr %p0, align 1
  ; Unboxed half-precision zero is classified as a quiet NaN.
  %p1 = getelementptr i64, ptr %out, i64 1
  store i64 512, ptr %p1, align 1
  ; Arithmetic checks boxing before using narrow operands.
  %p2 = getelementptr i64, ptr %out, i64 2
  store i64 2143289344, ptr %p2, align 1
  ; Sign injection checks boxing; negation then sets the NaN sign.
  %p3 = getelementptr i64, ptr %out, i64 3
  store i64 -4194304, ptr %p3, align 1
  ; Transfers out ignore boxing and preserve the low bits.
  %p4 = getelementptr i64, ptr %out, i64 4
  store i64 -2147434769, ptr %p4, align 1
  ; Half-precision transfers out also ignore boxing.
  %p5 = getelementptr i64, ptr %out, i64 5
  store i64 -16657, ptr %p5, align 1
  ; Stores ignore boxing as well.
  %p6 = getelementptr i64, ptr %out, i64 6
  store i64 -2147434769, ptr %p6, align 1
  ; Valid boxes preserve noncanonical NaN payloads on sign injection.
  %p7 = getelementptr i64, ptr %out, i64 7
  store i64 2143289472, ptr %p7, align 1
  ret void
}
