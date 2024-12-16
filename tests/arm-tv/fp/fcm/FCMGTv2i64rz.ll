define i32 @f(<2 x double> %0) {
  %2 = fcmp ogt <2 x double> %0, zeroinitializer
  %3 = extractelement <2 x i1> %2, i32 0
  %4 = extractelement <2 x i1> %2, i32 1
  %5 = and i1 %3, %4
  br i1 %5, label %6, label %7

6:                                                ; preds = %1
  ret i32 42

7:                                                ; preds = %1
  ret i32 88
}
