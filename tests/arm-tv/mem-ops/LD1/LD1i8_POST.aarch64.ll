; Function Attrs: noinline nounwind ssp uwtable
define <16 x i8> @f(ptr %0) {
  %2 = getelementptr inbounds i8, ptr %0, i64 1
  %3 = getelementptr inbounds i8, ptr %0, i64 2
  %4 = getelementptr inbounds i8, ptr %0, i64 3
  %5 = getelementptr inbounds i8, ptr %0, i64 6
  %6 = getelementptr inbounds i8, ptr %0, i64 7
  %7 = load i8, ptr %0, align 1
  %8 = load i8, ptr %2, align 1
  %9 = load i8, ptr %3, align 1
  %10 = load i8, ptr %4, align 1
  %11 = load i8, ptr %5, align 1
  %12 = load i8, ptr %6, align 1
  %13 = insertelement <16 x i8> undef, i8 %7, i32 0
  %14 = insertelement <16 x i8> %13, i8 %8, i32 1
  %15 = insertelement <16 x i8> %14, i8 %9, i32 2
  %16 = insertelement <16 x i8> %15, i8 %10, i32 3
  %17 = insertelement <16 x i8> %16, i8 %11, i32 6
  %18 = insertelement <16 x i8> %17, i8 %12, i32 7
  %19 = insertelement <16 x i8> %18, i8 0, i32 13
  %20 = insertelement <16 x i8> %19, i8 0, i32 14
  %21 = insertelement <16 x i8> %20, i8 0, i32 15
  ret <16 x i8> %21
}