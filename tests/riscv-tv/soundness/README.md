# RISC-V soundness audit

The original audit found seven soundness gaps, with eight reduced reproducers.
A rebuilt `backend-tv` at revision `074dd289` reported **1 correct
transformations** for every handwritten target here. Each target disagrees
with its LLVM source when
both are executed in QEMU. These are false approvals, not unsupported cases or
solver timeouts.

The stack-alignment gap is now fixed: `call-stack-align` requires a
counterexample. The other seven reproducers still track outstanding false
approvals. [BASELINE.md](BASELINE.md) records the coverage and timing comparison
for steps 1 and 2 of the [plan](PLAN.md).

1. **[P1] Symbolic LUI writes and the base register of `%lo` operations are
   ignored.**
   [The LUI handler](../../../backend_tv/riscv2llvm_insns.cpp#L389) treats
   `lui a0, %hi(g)` as a no-op. Consequently,
   `li a0, 42; lui a0, %hi(g); ret` validates against `ret i64 42`, although
   LUI always clears the low 12 bits and cannot return 42.
   Separately, [getPointerFromMCExpr](../../../backend_tv/riscv2llvm.cpp#L717)
   reconstructs the whole symbol address without reading the base register:
   `addi a0, zero, %lo(g); ret` validates as returning `@g`.
   Incorrect high/low pairing, omitted setup, and base-register clobbers can
   therefore escape validation for address materialization, loads, and stores.
   Reproducers: [reloc-high-clobber](reloc-high-clobber.riscvasm.ll) and
   [reloc-base](reloc-base.riscvasm.ll).
   Preserve the actual register write and address arithmetic, or reject
   symbolic relocations whose semantics cannot be established.

2. **[P1] Target optimization removes faulting machine loads.**
   [createLoad](../../../backend_tv/mc2llvm.h#L347) emits ordinary LLVM loads,
   and [liftFunc](../../../backend_tv/lifter.cpp#L100) runs O3 before validation.
   `ld zero, 0(a0); li a0, 42; ret` validates against a function returning 42
   without dereferencing its pointer argument. With a null argument the actual
   target faults. RISC-V requires loads into x0 to perform the access and raise
   exceptions. Using t0 as the discarded destination also reproduces this.
   [dead-load](dead-load.riscvasm.ll) is rejected with
   `-optimize-tgt=sroa`, isolating the loss to subsequent optimization.
   Access-validity obligations must survive optimization independently of
   whether the loaded value is used. Merely retaining register assignments
   does not address the problem.

3. **[P1, fixed] Calls did not check stack alignment.**
   Previously, `doCall` emitted the abstract LLVM call
   without checking architectural SP. The return path checked restoration, but
   that does not establish the required 16-byte alignment at the call.
   [call-stack-align](call-stack-align.riscvasm.ll) allocates eight bytes,
   saves ra, calls a function, then restores ra and SP. It falsely validated.
   A concrete callee returning `sp & 15` returns 8 for this target and 0 for
   the compiled source. The lifter now checks 16-byte alignment after
   architectural SP writes and before abstracting ordinary and tail calls.
   The original restoration checks remain in place. The `abi/sp-alignment-*`
   tests cover transient, conditional, and register-copy violations, together
   with valid calls, tails, stack-using leaves, and unreachable bad paths.

4. **[P1] Subtraction in symbolic data becomes addition.**
   [emitValueImpl](../../../backend_tv/streamerwrapper.cpp#L88) records the
   right-hand constant but discards the binary operator.
   Thus `.quad g-8` becomes a pointer to `g+8` in the lifted constant.
   [global-subtract](global-subtract.riscvasm.ll) loads that pointer and is
   falsely approved as returning `g+8`. Preserve the operator; reject
   unsupported expression forms rather than interpreting all of them as add.

5. **[P1] Alignment directives omit their padding bytes.**
   [emitValueToAlignment](../../../backend_tv/streamerwrapper.h#L233) only
   changes `curAlign`; it ignores the current offset, fill, and maximum padding.
   In an aligned table containing `.byte 1; .p2align 3; .byte 2`, the lifter
   puts 2 at offset 1 instead of offset 8.
   [global-align](global-align.riscvasm.ll) reads offset 1 and validates as
   returning 2; the actual assembly returns the padding byte, zero.
   Track section offsets and materialize the exact emitted padding.

6. **[P1] LLVM structure layout inserts bytes absent from assembly.**
   [lazyAddGlobal](../../../backend_tv/mc2llvm.cpp#L127) builds an unpacked
   LLVM structure from the sequence of bytes and symbolic pointers.
   `.byte 0; .quad g` has its pointer at offset 1 in assembly, but the lifted
   structure places it at offset 8. In
   [global-layout](global-layout.riscvasm.ll), seven trailing zero bytes keep
   the real eight-byte load at offset 8 within the table. The lifter returns
   `g`; with the concrete address used below, the actual load returns zero.
   Preserve byte offsets using packed layouts or equivalent explicit storage.

7. **[P1] Symbolic emission widths are discarded.**
   [emitValueImpl](../../../backend_tv/streamerwrapper.cpp#L68) ignores `Size`;
   [lazyAddGlobal](../../../backend_tv/mc2llvm.cpp#L121) subsequently emits every
   symbolic value as a full pointer. In
   [global-width](global-width.riscvasm.ll), `.word g; .word 1` is read with ld.
   The actual result is `g + 2^32` for the linked address below, but the lifter
   returns `g`. Record relocation widths and represent truncation accurately,
   or reject unsupported widths.

The load optimization and four data-emission findings involve shared MC/lifter
code. Their reproducers were validated on RISC-V; AArch64 was not tested here.

Run all validator reproductions and normal-codegen controls from the repo root:

```sh
python3 tests/riscv-tv/soundness/reproduce.py
```

To also execute the compiled LLVM source and handwritten assembly independently:

```sh
python3 tests/riscv-tv/soundness/reproduce.py --qemu
```

The latter requires an LLVM clang with RISC-V support, `ld.lld`, and
`qemu-system-riscv64` on PATH. Override their locations with `--clang`, `--linker`,
and `--qemu-bin`. The runner uses temporary files and a bare-metal QEMU `virt`
machine; no guest OS or cross sysroot is required. Link relaxation is disabled,
the stack starts aligned, and `g` is linked at `0x20001000`.

Observed results for all eight cases:

| Reproducer | Compiled source | Handwritten target |
| --- | --- | --- |
| reloc-high-clobber | 42 | `0x20001000` |
| reloc-base | `0x20001000` | 0 |
| dead-load, null argument | 42 | Load access fault, mcause=5 |
| call-stack-align | 0 | 8 |
| global-subtract | `0x20001008` | `0x20000ff8` |
| global-align | 2 | 0 |
| global-layout | `0x20001000` | 0 |
| global-width | `0x20001000` | `0x120001000` |

The runner verifies all 16 executions and all eight normal-codegen controls.
It requires a counterexample for `call-stack-align` and reproduces the seven
remaining false approvals. The SROA-only dead-load control must also produce a
counterexample. Completed verification summaries are checked exactly: a timeout,
unsupported input, or other error cannot stand in for a counterexample.
The remaining lit cases are marked XFAIL on their false-approval message.
Update the runner's `FIXED` set and remove each XFAIL as its fix lands.
