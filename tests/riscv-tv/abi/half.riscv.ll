define dso_local noundef half @addh(half noundef %x, half noundef %y) local_unnamed_addr #0 {
entry:
  %add = fadd half %x, %y
  ret half %add
}

