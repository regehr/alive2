; Function Attrs: nounwind
define void @f(<2 x i64> %0, <2 x i64> %1, ptr %2) {
  %4 = and <2 x i64> %0, <i64 281474976710655, i64 281474976710655>
  %5 = shl <2 x i64> %1, <i64 48, i64 48>
  %6 = or <2 x i64> %4, %5
  store <2 x i64> %6, ptr %2, align 16
  ret void
}