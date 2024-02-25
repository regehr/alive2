; ModuleID = '<stdin>'
source_filename = "<stdin>"

@g2 = external hidden global i64

define void @store2() {
  store i64 2, ptr @g2, align 4
  ret void
}
