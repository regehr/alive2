define void @f14(<4 x i32> %0, ptr %1) {
  %3 = extractelement <4 x i32> %0, i32 3
  store i32 %3, ptr %1, align 4
  ret void
}