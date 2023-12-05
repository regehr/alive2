; ModuleID = '<stdin>'
source_filename = "<stdin>"

; Function Attrs: nounwind ssp memory(read)
define <4 x i16> @fct2(ptr %0) {
  %2 = getelementptr inbounds i8, ptr %0, i64 3
  %3 = load <4 x i16>, ptr %2, align 8
  ret <4 x i16> %3
}