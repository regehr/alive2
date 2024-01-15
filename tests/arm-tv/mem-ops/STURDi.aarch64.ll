define void @f(<8 x i8> %0, ptr %1) {
  %3 = getelementptr inbounds <8 x i8>, ptr %1, i32 -1
  store <8 x i8> %0, ptr %3, align 8
  ret void
}