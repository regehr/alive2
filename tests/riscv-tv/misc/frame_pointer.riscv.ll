;; empty function whose prologue/epilogue spill ra and s0 to the stack;
;; the optimizer merges those spills into a memset and used to mark it
;; "tail", which is UB once the stack allocation becomes an alloca

define void @tf_0_foo() #0 {
entry:
  ret void
}

attributes #0 = { "frame-pointer"="all"}
