define dso_local noundef double @addd(double noundef %x, double noundef %y) local_unnamed_addr #0 {
entry:
  %add = fadd double %x, %y
  ret double %add
}

