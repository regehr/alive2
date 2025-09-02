define i32 @fld(ptr noundef %a) {
entry:
  %fp = load double, ptr %a, align 8
  %conv = fptosi double %fp to i32
  ret i32 %conv
}
