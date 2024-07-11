; Function Attrs: nounwind
define i64 @f(<1 x i64> %0, <2 x i64> %1) {
  %3 = extractelement <1 x i64> %0, i32 0
  %4 = extractelement <2 x i64> %1, i32 0
  %5 = extractelement <2 x i64> %1, i32 1
  %6 = sub i64 %3, %4
  %7 = sub i64 %6, %5
  ret i64 %7
}