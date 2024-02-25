@var = external global [2 x double]

define void @f(ptr %0) {
  %2 = load [2 x double], ptr %0, align 8
  store [2 x double] %2, ptr @var, align 8
  ret void
}