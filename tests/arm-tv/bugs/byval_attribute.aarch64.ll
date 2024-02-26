%0 = type { i16, i16 }

define i32 @f(ptr byval(%0) %0, i32 %1) {
  ret i32 %1
}

