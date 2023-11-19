
; Function Attrs: nounwind
define <4 x i16> @v_mvni16(ptr %0) #0 {
  %2 = load <4 x i16>, ptr %0, align 8
  %3 = xor <4 x i16> %2, <i16 -1, i16 -1, i16 -1, i16 -1>
  ret <4 x i16> %3
}

attributes #0 = { nounwind }
