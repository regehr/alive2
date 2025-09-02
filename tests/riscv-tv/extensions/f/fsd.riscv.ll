define void @fsd(ptr noundef %p) {
entry:
  %gep = getelementptr inbounds i8, ptr %p, i64 8
  %fp = load double, ptr %gep, align 8
  store double %fp, ptr %p, align 8
  ret void
}
