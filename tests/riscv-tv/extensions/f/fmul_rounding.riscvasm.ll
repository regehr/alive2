; FMUL honors directed rounding.
define void @fmul_rounding(ptr noundef %out) {
  ; (1+2^-23)^2, RUP, is 1+3*2^-23.
  %p0 = getelementptr i64, ptr %out, i64 0
  store i64 1065353219, ptr %p0, align 1
  ret void
}
