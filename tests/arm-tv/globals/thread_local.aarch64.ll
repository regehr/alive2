@dst_ie = external thread_local(initialexec) global i32

define i32 @f() {
  store i32 0, ptr @dst_ie, align 4
  ret i32 0
}

; CHECK: ERROR: thread_local