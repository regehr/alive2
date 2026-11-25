; CHECK: ERROR: the vtype for this vector instruction is not statically known
; calls do not preserve vtype, so it has to be set again after one
define void @test(ptr %dst) {
  call void @foo()
  call void @llvm.memset.p0.i64(ptr align 4 %dst, i8 0, i64 16, i1 false)
  ret void
}
declare void @foo()
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
