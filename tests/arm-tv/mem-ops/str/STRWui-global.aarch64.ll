; ModuleID = '<stdin>'
source_filename = "<stdin>"

@G = external hidden global i32

define void @foo() {
  store i32 17, ptr @G, align 4
  ret void
}
