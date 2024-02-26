define i64 @f(<2 x i64> %0) {
  %2 = alloca <2 x i64>, align 16
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = bitcast i32 0 to i32
  store <2 x i64> %0, ptr %2, align 16
  %6 = load <2 x i64>, ptr %2, align 16
  %7 = bitcast <2 x i64> %6 to <2 x i64>
  %8 = extractelement <2 x i64> %7, i32 0
  store i64 %8, ptr %4, align 8
  %9 = load i64, ptr %4, align 8
  store i64 %9, ptr %3, align 8
  br label %10

10:                                               ; preds = %1
  %11 = load i64, ptr %3, align 4
  ret i64 %11
}