define half @f(half %0, half %1) {
  %3 = fdiv half %1, %0
  ret half %3
}
