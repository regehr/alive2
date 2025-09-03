define void @fmv_w_x(i32 noundef signext %a, ptr noundef %p) {
entry:
  %cvt = bitcast i32 %a to float
  %add = fadd float %cvt, 0.0
  store float %add, ptr %p, align 4
  ret void
}
