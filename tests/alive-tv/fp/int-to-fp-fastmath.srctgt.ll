define double @src(i32 %x) {
  %r = sitofp nsz i32 %x to double
  ret double %r
}

define double @tgt(i32 %x) {
  %r = sitofp nsz i32 %x to double
  ret double %r
}
