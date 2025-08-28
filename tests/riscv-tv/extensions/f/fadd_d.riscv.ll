define signext i32 @fadd_d(i32 noundef signext %a) {
entry:
  %conv = sitofp i32 %a to double
  %add1 = fadd double %conv, %conv
  %conv2 = fptosi double %add1 to i32
  ret i32 %conv2
}
