; Function Attrs: nounwind
define void @extract_i64_1(ptr nocapture %0, <2 x i64> %1) {
  %3 = extractelement <2 x i64> %1, i32 1
  store i64 %3, ptr %0, align 1
  ret void
}