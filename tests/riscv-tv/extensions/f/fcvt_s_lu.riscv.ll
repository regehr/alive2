define void @fcvt_s_lu(i64 noundef %a, ptr noundef %p) {
entry:
  %conv = uitofp i64 %a to float
  store float %conv, ptr %p, align 4
  ret void
}
