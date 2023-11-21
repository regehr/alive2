
; Function Attrs: nounwind
define <2 x i64> @v_mvni128(ptr %0) #0 {
  %2 = load <2 x i64>, ptr %0, align 8
  %3 = xor <2 x i64> %2, <i64 -1, i64 -1>
  ret <2 x i64> %3
}

attributes #0 = { nounwind }
