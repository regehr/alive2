define void @f(<16 x i8> %0, ptr %1) {
  %3 = getelementptr inbounds <16 x i8>, ptr %1, i32 -1
  store <16 x i8> %0, ptr %3, align 16
  ret void
}