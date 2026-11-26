; CHECK: ERROR: calls that pass or return scalable vectors are not supported yet
declare void @consume(<vscale x 2 x i32>)
define void @test(<vscale x 2 x i32> %a) {
  call void @consume(<vscale x 2 x i32> %a)
  ret void
}
