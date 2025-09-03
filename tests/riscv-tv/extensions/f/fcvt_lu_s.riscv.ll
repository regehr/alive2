define i64 @fcvt_lu_s(ptr noundef %p) {
entry:
  %fp = load float, ptr %p, align 4
  %cvt = fptoui float %fp to i64
  ret i64 %cvt
}
