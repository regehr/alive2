define dso_local noundef half @sqrt(half noundef %x) local_unnamed_addr #0 {
entry:
  %1 = call half @llvm.sqrt.f16(half %x)
  ret half %1
}
