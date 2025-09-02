define void @fsw(i32 noundef signext %a, ptr noundef %p) {
entry:
  %conv = sitofp i32 %a to float
  store float %conv, ptr %p, align 4
  ret void
}
