; Function Attrs: nounwind memory(read)
define zeroext i16 @t6(ptr nocapture %0) {
  %2 = getelementptr inbounds i16, ptr %0, i32 -128
  %3 = load i16, ptr %2, align 2
  ret i16 %3
}