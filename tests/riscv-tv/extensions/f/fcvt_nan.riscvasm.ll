; RISC-V F: float-to-integer invalid-input table, including NaNs of either sign.
define void @fcvt_nan(ptr noundef %out) {
  ; FCVT.W.H maps NaN to the maximum integer; WU is sign-extended.
  %p0 = getelementptr i64, ptr %out, i64 0
  store i64 2147483647, ptr %p0, align 1
  ; FCVT.WU.H maps NaN to the maximum integer; WU is sign-extended.
  %p1 = getelementptr i64, ptr %out, i64 1
  store i64 -1, ptr %p1, align 1
  ; FCVT.L.H maps NaN to the maximum integer; WU is sign-extended.
  %p2 = getelementptr i64, ptr %out, i64 2
  store i64 9223372036854775807, ptr %p2, align 1
  ; FCVT.LU.H maps NaN to the maximum integer; WU is sign-extended.
  %p3 = getelementptr i64, ptr %out, i64 3
  store i64 -1, ptr %p3, align 1
  ; FCVT.W.S maps NaN to the maximum integer; WU is sign-extended.
  %p4 = getelementptr i64, ptr %out, i64 4
  store i64 2147483647, ptr %p4, align 1
  ; FCVT.WU.S maps NaN to the maximum integer; WU is sign-extended.
  %p5 = getelementptr i64, ptr %out, i64 5
  store i64 -1, ptr %p5, align 1
  ; FCVT.L.S maps NaN to the maximum integer; WU is sign-extended.
  %p6 = getelementptr i64, ptr %out, i64 6
  store i64 9223372036854775807, ptr %p6, align 1
  ; FCVT.LU.S maps NaN to the maximum integer; WU is sign-extended.
  %p7 = getelementptr i64, ptr %out, i64 7
  store i64 -1, ptr %p7, align 1
  ; FCVT.W.D maps NaN to the maximum integer; WU is sign-extended.
  %p8 = getelementptr i64, ptr %out, i64 8
  store i64 2147483647, ptr %p8, align 1
  ; FCVT.WU.D maps NaN to the maximum integer; WU is sign-extended.
  %p9 = getelementptr i64, ptr %out, i64 9
  store i64 -1, ptr %p9, align 1
  ; FCVT.L.D maps NaN to the maximum integer; WU is sign-extended.
  %p10 = getelementptr i64, ptr %out, i64 10
  store i64 9223372036854775807, ptr %p10, align 1
  ; FCVT.LU.D maps NaN to the maximum integer; WU is sign-extended.
  %p11 = getelementptr i64, ptr %out, i64 11
  store i64 -1, ptr %p11, align 1
  ret void
}
