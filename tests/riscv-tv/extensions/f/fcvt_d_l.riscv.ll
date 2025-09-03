define void @fcvt_d_l(i64 noundef %a, ptr noundef %p) {
entry:
  %conv = sitofp i64 %a to double
  store double %conv, ptr %p, align 8
  ret void
}
