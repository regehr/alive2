; CHECK: ERROR: the vtype for this vector instruction is not statically known
; unlike whole-register loads and stores, vmv<nr>r.v depends on vtype
define void @test(ptr %dst, ptr %src) {
  call void @llvm.memcpy.p0.p0.i64(ptr %dst, ptr %src, i64 16, i1 false)
  ret void
}
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
