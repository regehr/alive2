define void @fcvt_s_d(i8 noundef signext %a, ptr noundef %p2) {
entry:
  %fp = sitofp i8 %a to double
  %conv = fptrunc double %fp to float
  store float %conv, ptr %p2, align 4
  ret void
}
