; CHECK: ERROR: calls that pass or return vectors are not supported yet
declare void @consume(<4 x i32>)
define void @test(<4 x i32> %a) {
  call void @consume(<4 x i32> %a)
  ret void
}
