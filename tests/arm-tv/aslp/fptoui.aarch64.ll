define i16 @f(float %0) {
  %2 = call float @llvm.round.f32(float %0)
  %3 = fptoui float %2 to i16
  ret i16 %3
}

declare float @llvm.round.f32(float) nounwind memory(none)
