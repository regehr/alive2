; CHECK: ERROR: calls that pass or return scalable vectors are not supported yet
declare <vscale x 2 x i32> @produce()
define <vscale x 2 x i32> @test() {
  %r = call <vscale x 2 x i32> @produce()
  ret <vscale x 2 x i32> %r
}
