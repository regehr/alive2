define void @f(<4 x i32> %0, ptr %1) {
  %3 = getelementptr inbounds i32, ptr %1, i32 -6
  %4 = extractelement <4 x i32> %0, i32 0
  store i32 %4, ptr %3, align 4
  ret void
}