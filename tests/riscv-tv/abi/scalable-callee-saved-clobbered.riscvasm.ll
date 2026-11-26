; CHECK: 1 incorrect transformations
; a riscv_vector_cc function follows the vector calling convention
; variant, which makes v24-v31 callee-saved
define riscv_vector_cc <vscale x 2 x i32> @test(<vscale x 2 x i32> %a) {
  %r = add <vscale x 2 x i32> %a, %a
  ret <vscale x 2 x i32> %r
}
