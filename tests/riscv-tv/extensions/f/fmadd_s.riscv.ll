define signext i32 @fmadd_s(i32 noundef signext %a) {
entry:
  %conv_a = sitofp i32 %a to float
  %fma = call float @llvm.fma.f32(float %conv_a, float 2.0, float -1.0)
  %conv = fptosi float %fma to i32
  ret i32 %conv
}
