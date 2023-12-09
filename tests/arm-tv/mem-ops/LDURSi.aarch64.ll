define <2 x i32> @loadv2i32_offset(ptr %0) {
  %2 = getelementptr inbounds i8, ptr %0, i64 1
  %3 = load i32, ptr %2, align 4
  %4 = insertelement <2 x i32> zeroinitializer, i32 %3, i32 0
  ret <2 x i32> %4
}