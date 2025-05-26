; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local signext i32 @f(i32 noundef signext %0, i32 noundef signext %1) local_unnamed_addr #0 {
  %3 = icmp sgt i32 %0, %1
  %4 = zext i1 %3 to i32
  %5 = add nsw i32 %4, %0
  ret i32 %5
}
