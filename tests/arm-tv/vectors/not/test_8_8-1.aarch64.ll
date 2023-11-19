
; Function Attrs: nounwind
define <8 x i8> @v_mvni8(ptr %0) #0 {
  %2 = load <8 x i8>, ptr %0, align 8
  %3 = xor <8 x i8> %2, <i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1, i8 -1>
  ret <8 x i8> %3
}

attributes #0 = { nounwind }
