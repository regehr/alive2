; CHECK: 1 correct transformations
; LLVM applies the vector calling convention variant only to
; riscv_vector_cc functions, so a C calling convention function may
; clobber v24 even though it takes a vector argument
define <vscale x 2 x i32> @test(<vscale x 2 x i32> %a) {
  %r = add <vscale x 2 x i32> %a, %a
  ret <vscale x 2 x i32> %r
}
