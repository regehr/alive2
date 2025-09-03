define void @fcvt_s_l(i64 noundef %a, ptr noundef %p) {
entry:
  %conv = sitofp i64 %a to float
  store float %conv, ptr %p, align 4
  ret void
}
