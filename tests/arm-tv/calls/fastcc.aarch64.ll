; ModuleID = 'reduced.bc'
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

; Function Attrs: strictfp
define void @_Z8msgcountv() #0 {
entry:
  call fastcc void @_ZL6_countb()
  ret void
}

; Function Attrs: strictfp memory(readwrite, argmem: read, inaccessiblemem: none)
declare fastcc void @_ZL6_countb() #1

attributes #0 = { strictfp }
attributes #1 = { strictfp memory(readwrite, argmem: read, inaccessiblemem: none) }
