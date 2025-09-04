define dso_local noundef float @adds(float noundef %x, float noundef %y) local_unnamed_addr #0 {
entry:
  %add = fadd float %x, %y
  ret float %add
}

