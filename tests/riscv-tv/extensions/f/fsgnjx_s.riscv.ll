define signext i32 @fsgnjx_s(i32 noundef signext %a) {
entry:
  %conv = sitofp i32 %a to float
  %fabs = call float @llvm.fabs.f32(float %conv)
  %conv2 = fptosi float %fabs to i32
  ret i32 %conv2
}
