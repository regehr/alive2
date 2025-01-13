declare ptr @f0()

define ptr @f() {
  %1 = tail call noalias ptr @f0()
  ret ptr %1
}
