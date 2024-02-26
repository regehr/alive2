; Function Attrs: nounwind
define void @f(ptr %0, ptr %1, ptr %2) {
  %4 = load <16 x i64>, ptr %0, align 64
  %5 = shufflevector <16 x i64> %4, <16 x i64> poison, <8 x i32> <i32 0, i32 2, i32 4, i32 6, i32 8, i32 10, i32 12, i32 14>
  store <8 x i64> %5, ptr %1, align 64
  ret void
}