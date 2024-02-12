; ModuleID = 'reduced.bc'
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx14.0.0"

; Function Attrs: strictfp
define signext i16 @_ZNK11xercesc_2_723DOMDocumentFragmentImpl19compareTreePositionEPKNS_7DOMNodeE() #0 {
entry:
  %call = call i16 @_ZNK11xercesc_2_711DOMNodeImpl19compareTreePositionEPKNS_7DOMNodeE()
  ret i16 %call
}

declare signext i16 @_ZNK11xercesc_2_711DOMNodeImpl19compareTreePositionEPKNS_7DOMNodeE()

attributes #0 = { strictfp }
