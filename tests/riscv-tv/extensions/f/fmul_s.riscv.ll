define signext i32 @fmul_s(i32 noundef signext %a) {
entry:
  %conv = sitofp i32 %a to float
  %neg = fmul float %conv, 6.000000e+00
  %conv2 = fptosi float %neg to i32
  ret i32 %conv2
}
