; FP narrowing honors the instruction rounding mode; widening remains exact.
define void @fcvt_fp_rounding(ptr noundef %out) {
  ; D to S, RUP: 1+2^-24 rounds up to the next single.
  %p0 = getelementptr i64, ptr %out, i64 0
  store i64 1065353217, ptr %p0, align 1
  ; S to H, RMM: 1+2^-11 rounds away from zero.
  %p1 = getelementptr i64, ptr %out, i64 1
  store i64 15361, ptr %p1, align 1
  ; H to D, RTZ: widening of 1 is exact.
  %p2 = getelementptr i64, ptr %out, i64 2
  store i64 4607182418800017408, ptr %p2, align 1
  ret void
}
