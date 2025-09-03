define void @fmv_d_x(i64 noundef %a, ptr noundef %p) {
entry:
  %cvt = bitcast i64 %a to double
  %add = fadd double %cvt, 0.0
  store double %add, ptr %p, align 8
  ret void
}
