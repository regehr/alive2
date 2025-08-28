define signext i32 @fadd_s(i32 noundef signext %a) {
entry:
  %conv = sitofp i32 %a to float
  %add1 = fadd float %conv, 6.000000e+00
  %conv2 = fptosi float %add1 to i32
  ret i32 %conv2
}
