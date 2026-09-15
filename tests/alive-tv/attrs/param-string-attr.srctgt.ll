; A parameter can carry a string attribute, and handleParamAttrs() used to
; reach getKindAsEnum() on it, which asserts. handleRetAttrs() and
; handleFnAttrs() already screen those out.

define i32 @src(i64 "foo bar" %x) {
  ret i32 41
}

define i32 @tgt(i64 "foo bar" %x) {
  ret i32 41
}

; ERROR: Unsupported attribute: "foo bar"

; SKIP-IDENTITY
