; Function Attrs: strictfp
define void @conjugate_qt(ptr %q, ptr %p) #0 {
entry:
  %0 = load <2 x double>, ptr %p, align 4
  %1 = fneg <2 x double> %0
  store <2 x double> %1, ptr %q, align 4
  ret void
}

attributes #0 = { strictfp }