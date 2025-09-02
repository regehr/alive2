define void @fcvt_d_s(ptr noundef %p1, ptr noundef %p2) {
entry:
  %fp = load float, ptr %p1, align 4
  %conv = fpext float %fp to double
  store double %conv, ptr %p2, align 8
  ret void
}
