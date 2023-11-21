
; Function Attrs: nounwind
define <16 x i8> @v_mvni8(ptr %0) #0 {
  %2 = load <16 x i8>, ptr %0, align 16
  %3 = xor <16 x i8> %2, <i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1>
  ret <16 x i8> %3
}

attributes #0 = { nounwind }
