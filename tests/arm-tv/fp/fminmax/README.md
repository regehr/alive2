# AArch64 floating-point min/max tests

The scalar lowering matrix covers all six LLVM intrinsic families at float and
double precision, with no fast-math flags, with `nnan`, and with `nsz`.
The older instruction-named tests also exercise compound expressions and calls.

The assembly tests independently specify FMIN, FMAX, FMINNM, and FMAXNM using
integer ordering and classification of the operand bits. They check signed
zeros, quiet and signaling NaNs, payload selection, and quieting. The reference
is Arm's ISA_A64_xml_A_profile-2026-06_mc release, shared pseudocode FPMin, FPMax,
FPMinNum, FPMaxNum, FPProcessNaNs, and FPProcessNaN. FPCR is assumed to have
AH=DN=FZ=FZ16=0, with exceptions disabled; FPSR exception flags are not checked.
In particular, FMINNM/FMAXNM propagate a signaling NaN even with a numeric
operand, unlike RISC-V FMIN/FMAX.

The `wrong-*` assembly fixtures deliberately violate one of these properties
and must produce a value-mismatch counterexample. Half inputs and vector min/max
instruction support are outside this scalar test matrix.

The existing `fmaxnmsrr.aarch64.ll` test is an expected failure: LLVM lowers
`(a >= c) | (b >= c)` through FMAXNM, which returns a quiet NaN when `a=c=+0`
and `b` is a signaling NaN. The subsequent comparison returns false even though
the source returns true. This counterexample was confirmed on AArch64 hardware.
