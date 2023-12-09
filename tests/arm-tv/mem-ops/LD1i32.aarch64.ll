define <4 x i32> @pinsrd_from_shufflevector_i32(<4 x i32> %0, ptr nocapture readonly %1) {
  %3 = load <4 x i32>, ptr %1, align 16
  %4 = shufflevector <4 x i32> %0, <4 x i32> %3, <4 x i32> <i32 0, i32 1, i32 2, i32 4>
  ret <4 x i32> %4
}