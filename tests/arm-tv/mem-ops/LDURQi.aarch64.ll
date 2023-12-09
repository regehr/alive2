; Function Attrs: nounwind ssp memory(read)
define <16 x i8> @fct7(ptr %0) {
  %2 = getelementptr inbounds i8, ptr %0, i64 3
  %3 = load <16 x i8>, ptr %2, align 16
  ret <16 x i8> %3
}