; TEST-ARGS: --global-isel

define i1 @f(i1 %0) {
  %2 = shl i1 true, %0
  %3 = select i1 %0, i1 true, i1 %2
  ret i1 %3
}
