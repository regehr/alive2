define i32 @f(<2 x double> %0, <2 x double> %1) {
  %3 = fcmp ogt <2 x double> %0, %1
  %4 = extractelement <2 x i1> %3, i64 0
  %5 = extractelement <2 x i1> %3, i64 1
  %6 = select i1 %4, i1 %5, i1 false
  %7 = select i1 %6, i32 42, i32 99
  ret i32 %7
}
