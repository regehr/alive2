
; Function Attrs: nounwind
define <4 x i32> @v_mvni16(ptr %0) #0 {
  %2 = load <4 x i32>, ptr %0, align 8
  %3 = xor <4 x i32> %2, <i32 -1, i32 -1, i32 -1, i32 -1>
  ret <4 x i32> %3
}

attributes #0 = { nounwind }
