; TEST-ARGS: -disable-undef-input -disable-poison-input
; SKIP-IDENTITY
target triple = "aarch64-unknown-linux-gnu"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"

; ---- memory_load ----
define i32 @src(ptr %p) {
  %r = load i32, ptr %p
  ret i32 %r
}

define i32 @tgt(ptr %p) {
  %r = call i32 asm "ldr ${0:w}, $1", "=r,*m"(ptr elementtype(i32) %p)
  ret i32 %r
}

; ---- memory_store ----
define void @src_memory_store(ptr %p, i32 %val) {
  store i32 %val, ptr %p
  ret void
}

define void @tgt_memory_store(ptr %p, i32 %val) {
  call void asm "str ${0:w}, $1", "r,*m"(i32 %val, ptr elementtype(i32) %p)
  ret void
}

; ---- read_modify_write ----
define void @src_read_modify_write(ptr %p) {
  %v = load i32, ptr %p
  %inc = add i32 %v, 1
  store i32 %inc, ptr %p
  ret void
}

define void @tgt_read_modify_write(ptr %p) {
  call void asm "ldr w8, [$0]\0Aadd w8, w8, #1\0Astr w8, [$0]", "r,~{w8},~{memory}"(ptr %p)
  ret void
}

; ---- compiler_barrier ----
define i32 @src_compiler_barrier(i32 %a) {
  ret i32 %a
}

define i32 @tgt_compiler_barrier(i32 %a) {
  call void asm sideeffect "", "~{memory}"()
  ret i32 %a
}
