; CHECK: 1 correct transformations
; LLVM applies the vector calling convention variant only to
; riscv_vector_cc functions, so a C calling convention function may
; clobber v1 even though it takes a fixed-length vector argument
define <4 x i32> @test(<4 x i32> %a) {
  %r = add <4 x i32> %a, %a
  ret <4 x i32> %r
}
