; ModuleID = '<stdin>'
source_filename = "<stdin>"

define <16 x i8> @loadv16i8_noffset(ptr %0) {
  %2 = getelementptr inbounds i8, ptr %0, i64 -1
  %3 = load i8, ptr %2, align 1
  %4 = insertelement <16 x i8> zeroinitializer, i8 %3, i32 0
  ret <16 x i8> %4
}