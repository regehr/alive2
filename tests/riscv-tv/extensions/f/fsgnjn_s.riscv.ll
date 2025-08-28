define signext i32 @fsgnjn_s(i32 noundef signext %a) {
entry:
  %conv = sitofp i32 %a to float
  %neg = fneg float %conv
  %conv2 = fptosi float %neg to i32
  ret i32 %conv2
}
