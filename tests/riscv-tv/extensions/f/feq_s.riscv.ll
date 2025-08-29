define signext i32 @feq_s(i32 noundef signext %a) {
entry:
  %conv = sitofp i32 %a to float
  %cmp = fcmp oeq float %conv, 0.0
  %conv2 = zext i1 %cmp to i32
  ret i32 %conv2
}
