; XFAIL: *
;; Moved here becuase codegen still contains c_addi4spn,
;; and we should be able to handle this. Possibly an Alive issue

define void @tf_0_foo() #0 {
entry:
  ret void
}

attributes #0 = { "frame-pointer"="all"}
