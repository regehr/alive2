; v24 is callee-saved under the vector calling convention variant, but it
; may be used as long as its value is put back before returning
define riscv_vector_cc <vscale x 2 x i32> @test(<vscale x 2 x i32> %a) {
  %r = add <vscale x 2 x i32> %a, %a
  ret <vscale x 2 x i32> %r
}
