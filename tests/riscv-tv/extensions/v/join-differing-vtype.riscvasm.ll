; CHECK: ERROR: the vtype for this vector instruction is not statically known
define void @test(ptr %dst, i64 %c) {
  ret void
}
