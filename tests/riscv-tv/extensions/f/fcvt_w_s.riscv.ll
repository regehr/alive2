define signext i32 @fcvt_w_s(ptr noundef %p) {
entry:
  %fp = load float, ptr %p, align 4
  %cvt = fptosi float %fp to i32
  ret i32 %cvt
}
