; an m2 argument needs an even-numbered group, so after an m1 argument in
; v8 it takes v10-v11; an m2 result comes back in v8-v9
define <vscale x 4 x i32> @test(<vscale x 2 x i32> %a, <vscale x 4 x i32> %b) {
  %r = add <vscale x 4 x i32> %b, %b
  ret <vscale x 4 x i32> %r
}
