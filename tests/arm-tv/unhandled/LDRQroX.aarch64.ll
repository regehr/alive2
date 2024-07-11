@GlobLd128 = external dso_local local_unnamed_addr global [20 x fp128], align 16
@GlobSt128 = external dso_local local_unnamed_addr global [20 x fp128], align 16

define dso_local void @testGlob128PtrPlusVar(i64 %0) {
  %2 = getelementptr inbounds [20 x fp128], ptr @GlobLd128, i64 0, i64 %0
  %3 = load fp128, ptr %2, align 16
  %4 = getelementptr inbounds [20 x fp128], ptr @GlobSt128, i64 0, i64 %0
  store fp128 %3, ptr %4, align 16
  ret void
}