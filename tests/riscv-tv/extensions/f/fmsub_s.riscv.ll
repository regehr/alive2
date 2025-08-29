define signext i32 @fmsub_s(i32 noundef signext %a) {
entry:
  %conv_a = sitofp i32 %a to float
  %fneg = fneg float %conv_a
  %fma = call float @llvm.fma.f32(float %conv_a, float 1.5, float %fneg)
  %conv = fptosi float %fma to i32
  ret i32 %conv
}
