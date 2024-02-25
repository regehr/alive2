define <2 x i64> @testllv(<2 x i64> returned %0, ptr nocapture %1, i64 %2) local_unnamed_addr {
  %4 = extractelement <2 x i64> %0, i32 0
  %5 = getelementptr inbounds i64, ptr %1, i64 %2
  store i64 %4, ptr %5, align 8
  ret <2 x i64> %0
}