define void @test_splat(i32 %0, i32 %1, ptr %2) {
  %4 = load i32, ptr %2, align 4
  %5 = getelementptr inbounds i32, ptr %2, i64 1
  %6 = getelementptr inbounds i32, ptr %2, i64 2
  %7 = getelementptr inbounds i32, ptr %2, i64 3
  %8 = insertelement <4 x i32> undef, i32 %4, i32 0
  %9 = insertelement <4 x i32> %8, i32 %4, i32 1
  %10 = insertelement <4 x i32> %9, i32 %4, i32 2
  %11 = insertelement <4 x i32> %10, i32 %4, i32 3
  store <4 x i32> %11, ptr %5, align 4
  store i32 %0, ptr %7, align 4
  store i32 %1, ptr %6, align 4
  ret void
}