define void @f(ptr %0) {
  %2 = alloca <2 x i64>, align 16
  %3 = alloca <2 x i64>, align 16
  %4 = alloca <2 x i64>, align 16
  store <2 x i64> zeroinitializer, ptr %4, align 16
  %5 = load <2 x i64>, ptr %4, align 16
  %6 = load i8, ptr %0, align 4
  %7 = insertelement <16 x i8> undef, i8 %6, i32 0
  %8 = shufflevector <16 x i8> %7, <16 x i8> undef, <16 x i32> zeroinitializer
  %9 = bitcast <16 x i8> %8 to <2 x i64>
  store <2 x i64> %5, ptr %2, align 16
  store <2 x i64> %9, ptr %3, align 16
  ret void
}