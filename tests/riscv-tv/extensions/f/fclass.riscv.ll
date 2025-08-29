define signext i32 @fclass_s(i32 noundef signext %a) {
entry:
  %conv_a = sitofp i32 %a to float
  %test = call i1 @llvm.is.fpclass(float %conv_a, i32 256)
  %conv = zext i1 %test to i32
  ret i32 %conv
}
