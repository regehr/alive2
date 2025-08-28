define signext i32 @fsub_s(i32 noundef signext %a) {
entry:
  %conv = sitofp i32 %a to float
  %sub = fsub float %conv, %conv ; fsub %x, c will be canonicalized into fadd %x, -c
  %conv2 = fptosi float %sub to i32
  ret i32 %conv2
}
