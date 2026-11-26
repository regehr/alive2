; CHECK: ERROR: only scalable vectors of integers are supported
define <vscale x 2 x double> @test(<vscale x 2 x double> %a) {
  ret <vscale x 2 x double> %a
}
