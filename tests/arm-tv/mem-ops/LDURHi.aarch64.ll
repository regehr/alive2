define <4 x i16> @loadv4i16_offset(ptr %0) {
  %2 = getelementptr inbounds i8, ptr %0, i64 1
  %3 = load i16, ptr %2, align 2
  %4 = insertelement <4 x i16> zeroinitializer, i16 %3, i32 0
  ret <4 x i16> %4
}