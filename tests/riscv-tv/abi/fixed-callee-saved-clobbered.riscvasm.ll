; CHECK: 1 incorrect transformations
; a riscv_vector_cc function with a fixed-length vector argument follows
; the vector calling convention variant, which makes v1-v7 callee-saved
define riscv_vector_cc <4 x i32> @test(<4 x i32> %a) {
  %r = add <4 x i32> %a, %a
  ret <4 x i32> %r
}
