define <2 x i64> @s2v_test4(ptr nocapture readonly %0, <2 x i64> %1) {
  %3 = getelementptr inbounds i64, ptr %0, i64 1
  %4 = load i64, ptr %3, align 8
  %5 = insertelement <2 x i64> %1, i64 %4, i32 0
  ret <2 x i64> %5
}