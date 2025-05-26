; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local noundef signext i32 @f(i32 noundef signext %0, i32 noundef signext %1) local_unnamed_addr #0 {
  %3 = sub i32 %0, %1
  ret i32 %3
}
