
@glob = external local_unnamed_addr global i16, align 2

define void @test_igtss_sext_z_store(i16 signext %0) {
  %2 = icmp sgt i16 %0, 0
  %3 = sext i1 %2 to i16
  store i16 %3, ptr @glob, align 2
  ret void
}
