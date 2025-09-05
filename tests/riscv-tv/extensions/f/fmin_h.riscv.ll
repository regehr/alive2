define dso_local noundef half @mins(half noundef %x, half noundef %y, half noundef %z) local_unnamed_addr #0 {
entry:
  %1 = call half @llvm.minnum.f16(half %x, half %y)
  %2 = call half @llvm.minnum.f16(half %z, half %1)
  ret half %2
}
