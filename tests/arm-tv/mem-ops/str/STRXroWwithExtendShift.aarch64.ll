define i64 @store_R_d(ptr %0, i32 %1, i64 %2) local_unnamed_addr {
  %4 = sext i32 %1 to i64
  %5 = getelementptr inbounds i64, ptr %0, i64 %4
  store i64 %2, ptr %5, align 4
  ret i64 0
}