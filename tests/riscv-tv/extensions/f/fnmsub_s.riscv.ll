define signext i32 @fnmsub_s(i32 noundef signext %a) {
entry:
  %trunc_a = trunc nsw i32 %a to i4
  %conv_a = sitofp i4 %trunc_a to float
  %fneg = fneg float %conv_a
  %fma = call float @llvm.fma.f32(float %conv_a, float %fneg, float %conv_a)
  %conv = fptosi float %fma to i32
  ret i32 %conv
}
