
; Function Attrs: nounwind
define <1 x i64> @v_mvni64(ptr %0) #0 {
  %2 = load <1 x i64>, ptr %0, align 8
  %3 = xor <1 x i64> %2, <i64 -1>
  ret <1 x i64> %3
}

attributes #0 = { nounwind }
