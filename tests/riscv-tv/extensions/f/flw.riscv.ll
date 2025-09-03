define signext i32 @flw(ptr noundef %a) {
entry:
  %fp = load float, ptr %a, align 4
  %conv = fptosi float %fp to i32
  ret i32 %conv
}
