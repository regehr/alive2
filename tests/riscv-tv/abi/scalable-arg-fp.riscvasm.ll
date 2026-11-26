; a scalable FP vector is passed and returned in v8-v9
define <vscale x 2 x double> @test(<vscale x 2 x double> %a) {
  ret <vscale x 2 x double> %a
}
