define void @f(<8 x i16> %0, ptr %1) {
  %3 = getelementptr inbounds i16, ptr %1, i32 -6
  %4 = extractelement <8 x i16> %0, i32 0
  store i16 %4, ptr %3, align 2
  ret void
}