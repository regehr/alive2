; Function Attrs: strictfp
define internal i32 @pose_flip_quats_exec(ptr %0, ptr %1) #0 {
entry:
  %2 = load <4 x float>, ptr %1, align 4
  %3 = fneg <4 x float> %2
  store <4 x float> %3, ptr %0, align 4
  ret i32 0
}

attributes #0 = { strictfp }