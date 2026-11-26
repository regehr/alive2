define <vscale x 1 x double> @test(<vscale x 1 x double> %x, <vscale x 1 x double> %y) {
  %r = fadd <vscale x 1 x double> %x, %y
  ret <vscale x 1 x double> %r
}
