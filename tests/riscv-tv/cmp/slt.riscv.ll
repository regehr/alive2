; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local noundef signext i32 @f(i32 noundef signext %0, i32 noundef signext %1) local_unnamed_addr #0 {
  %3 = icmp slt i32 %0, %1
  %4 = zext i1 %3 to i32
  %5 = icmp slt i32 %1, %0
  %6 = zext i1 %5 to i32
  %7 = add i32 %1, %0
  %8 = add i32 %7, %6
  %9 = add i32 %8, %4
  ret i32 %9
}
