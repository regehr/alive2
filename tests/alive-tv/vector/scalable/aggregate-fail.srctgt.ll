; ERROR: Value mismatch

define <vscale x 2 x i8> @src(<vscale x 2 x i8> noundef %a, <vscale x 2 x i8> noundef %b) {
  %x = insertvalue { i32, <vscale x 2 x i8> } poison, <vscale x 2 x i8> %a, 1
  %r = extractvalue { i32, <vscale x 2 x i8> } %x, 1
  ret <vscale x 2 x i8> %r
}

define <vscale x 2 x i8> @tgt(<vscale x 2 x i8> noundef %a, <vscale x 2 x i8> noundef %b) {
  ret <vscale x 2 x i8> %b
}
