define signext i32 @fsgnj_s(i32 noundef signext %a) {
entry:
  %conv = sitofp i32 %a to float
  %copysign = call float @llvm.copysign.f32(float 6.000000e+00, float %conv)
  %conv2 = fptosi float %copysign to i32
  ret i32 %conv2
}
