define void @fcvt_d_w(i32 noundef signext %a, ptr noundef %p) {
entry:
  %conv = sitofp i32 %a to double
  store double %conv, ptr %p, align 8
  ret void
}
