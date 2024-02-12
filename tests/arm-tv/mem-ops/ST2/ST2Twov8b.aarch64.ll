define void @f(ptr %0) {
  %2 = load <16 x i8>, ptr %0, align 16
  %3 = shufflevector <16 x i8> %2, <16 x i8> %2, <16 x i32> <i32 0, i32 0, i32 1, i32 1, i32 2, i32 2, i32 3, i32 3, i32 4, i32 4, i32 5, i32 5, i32 6, i32 6, i32 7, i32 7>
  store <16 x i8> %3, ptr %0, align 16
  ret void
}