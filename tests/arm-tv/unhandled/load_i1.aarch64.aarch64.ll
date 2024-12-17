target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

define i8 @f(ptr %p) {
  %x = load i1, ptr %p, align 4
  %y = zext i1 %x to i8
  ret i8 %y
}

