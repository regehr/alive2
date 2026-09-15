; ERROR: Target's return value is more undefined

; Same as sitofp-nsz-fail, elementwise.

define <4 x double> @src(<4 x i32> %x) {
  %r = sitofp <4 x i32> %x to <4 x double>
  ret <4 x double> %r
}

define <4 x double> @tgt(<4 x i32> %x) {
  %r = sitofp nsz <4 x i32> %x to <4 x double>
  ret <4 x double> %r
}
