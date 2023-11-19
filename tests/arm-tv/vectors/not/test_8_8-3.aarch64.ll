
; Function Attrs: nounwind
define <2 x i32> @v_mvni32(ptr %0) #0 {
  %2 = load <2 x i32>, ptr %0, align 8
  %3 = xor <2 x i32> %2, <i32 -1, i32 -1>
  ret <2 x i32> %3
}

attributes #0 = { nounwind }
