define signext i32 @fmv_x_w(ptr noundef %p) {
entry:
  %fp = load float, ptr %p, align 4
  %add = fadd float %fp, 0.0
  %cvt = bitcast float %add to i32
  ret i32 %cvt
}
