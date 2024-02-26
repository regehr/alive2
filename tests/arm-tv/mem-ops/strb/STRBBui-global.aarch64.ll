; ModuleID = '<stdin>'
source_filename = "<stdin>"

@glob = external dso_local local_unnamed_addr global i8, align 1

define dso_local void @test_ieqsc_z_store(i8 signext %0) {
  %2 = icmp eq i8 %0, 0
  %3 = zext i1 %2 to i8
  store i8 %3, ptr @glob, align 1
  ret void
}
