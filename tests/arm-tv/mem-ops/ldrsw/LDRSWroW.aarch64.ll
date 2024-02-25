define <2 x i64> @f(ptr nocapture readonly %0, <2 x i64> %1, i32 signext %2) {
  %4 = sext i32 %2 to i64
  %5 = getelementptr inbounds i32, ptr %0, i64 %4
  %6 = load i32, ptr %5, align 4
  %7 = sext i32 %6 to i64
  %8 = insertelement <2 x i64> %1, i64 %7, i32 0
  ret <2 x i64> %8
}