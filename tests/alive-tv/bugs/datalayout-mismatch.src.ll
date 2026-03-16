target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%S = type { i8, i64 }

define i64 @f(ptr %p) {
entry:
  %s = load %S, ptr %p, align 8
  %x = extractvalue %S %s, 1
  ret i64 %x
}

; ERROR: Modules have different data layouts
