define dso_local void @st_reg_vector(ptr nocapture %0, i64 %1, <16 x i8> %2) {
  %4 = getelementptr inbounds i8, ptr %0, i64 %1
  store <16 x i8> %2, ptr %4, align 16
  ret void
}