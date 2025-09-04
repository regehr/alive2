define dso_local noundef float @f(float noundef %a, i32 noundef signext %b, float noundef %c) local_unnamed_addr #0 {
entry:
  %conv = sitofp i32 %b to float
  %add = fadd float %a, %conv
  %add1 = fadd float %c, %add
  ret float %add1
}
