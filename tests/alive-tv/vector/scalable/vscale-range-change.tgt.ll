define <vscale x 2 x i8> @f(<vscale x 2 x i8> %x) vscale_range(2,4) {
  ret <vscale x 2 x i8> %x
}
