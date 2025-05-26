; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local signext i32 @f(i32 noundef signext %0, i32 noundef signext %1) local_unnamed_addr #0 {
  %3 = sub nsw i32 %0, %1
  %4 = icmp slt i32 %3, -100
  %5 = icmp sgt i32 %0, %1
  %6 = xor i1 %5, %4
  %7 = sub i32 0, %0
  %8 = select i1 %6, i32 %0, i32 %7
  %9 = add i32 %8, %1
  ret i32 %9
}
