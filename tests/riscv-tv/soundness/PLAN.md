# Plan for closing the RISC-V soundness gaps

The objective is to reject the eight incorrect targets while continuing to
validate their correct counterparts. The fixes must preserve machine behavior
through lifting, optimization, and conversion to Alive2. Explicitly rejecting
an unsupported assembly construct is acceptable containment, but it must be
reported separately from restoring full support.

Steps 1 and 2 are implemented; see [BASELINE.md](BASELINE.md) for the validation
record. Steps 3 through 7 remain planned. Use the following order of reviewable
changes.

1. **Establish the regression and coverage baseline.**

   Keep the eight reduced source/assembly pairs and the QEMU comparisons.
   Capture the current RISC-V suite results, plus AArch64 results for changes
   to shared code. Record successful validations, counterexamples, unsupported
   inputs, timeouts, and representative run times separately.

   Change each fixed test from XFAIL to an ordinary rejection test as its fix
   lands. Update `reproduce.py` incrementally to expect that case's new result.
   Keep the normal-codegen controls. An unsupported diagnostic needs its own
   explicit expectation; neither a timeout nor an arbitrary error counts as
   detecting the miscompilation.

2. **Enforce the stack alignment contract.**

   In `riscv2llvm`, express the supported ABI's 16-byte SP alignment as an
   explicit assertion. Check SP before calls, including tail calls, and enforce
   the ABI's requirement to maintain alignment on architectural SP updates.
   Preserve the existing restoration checks.

   Cover aligned and misaligned ordinary calls, tail calls, conditional paths,
   and SP restoration. Keep a valid stack-using leaf function as a control.
   Use the concrete `check_alignment` callee in the QEMU runner to confirm the
   original failure is eliminated by validation. This is the smallest fix and
   should land first.

3. **Protect machine memory accesses from LLVM's UB-based optimization.**

   Add explicit machine-load and machine-store interfaces in the shared lifter.
   Keep accesses to internal register-storage allocas as ordinary LLVM memory
   operations so SROA and register promotion remain effective.

   Preferred design: represent architectural accesses as opaque helper calls
   throughout LLVM optimization. Use memory effects that preserve the access,
   its control dependence, and its ordering relative to relevant loads,
   stores, and calls. Do not give the optimizer attributes that imply the
   address is valid or permit dead-call elimination. Track helper declarations
   by identity, following the existing unknown-value helper mechanism.

   Expand those calls into actual loads/stores in `fixupOptimizedTgt`, after
   general LLVM optimization. Alive2 then checks access validity using its
   existing load/store semantics. Ensure no later optimization can remove the
   reconstructed accesses; inspect the existing post-fixup pointer rewrites
   as part of this invariant.

   Protect stores as well as loads: otherwise an overwritten invalid store can
   expose the same class of problem through dead-store elimination. This is a
   deliberate extension of the reported dead-load fix, needed for a consistent
   architectural memory interface. Apply the shared mechanism to RISC-V first;
   changing AArch64 access lowering should be a separately validated follow-up.

   An ordinary load plus an opaque access check is insufficient: the load
   still exposes UB that LLVM can use to discard a bad path. A volatile marker
   alone likewise does not establish that optimization preserves all failure
   behavior. Making all register-file traffic opaque would unnecessarily
   damage optimization and solver performance.

   Prototype this change before widening it across the lifter. Validate dead
   loads into x0 and temporaries, FP loads, overwritten invalid stores,
   conditional invalid accesses, loops, and accesses separated by stores or
   calls that change memory lifetime. Check valid unused accesses too. Compare
   O3 with a restricted optimization pipeline, and measure spill-heavy and
   call-heavy cases. If preserving whole accesses is prohibitively expensive,
   design a narrower obligation mechanism only with a demonstrated argument
   that LLVM cannot exploit the remaining exposed memory operations.

4. **Preserve symbolic data expressions, widths, and byte layout.**

   Extend the assembly-data representation to record a symbolic expression's
   emitted byte width and offset. Normalize supported `symbol`, `symbol+C`,
   and `symbol-C` expressions with explicit-width arithmetic. Reject expression
   forms that are not modeled; do not reinterpret their operators.

   Lower data to packed LLVM layouts with explicit byte chunks and symbolic
   fields. Retain full-width pointers where possible for Alive2's pointer
   model. Represent narrower absolute relocations with the corresponding
   integer truncation, subject to the actual relocation rules. Validate the
   constant-expression path through `llvm2alive`; if a width cannot yet be
   represented correctly, reject precisely that form.

   This closes `global-subtract`, `global-layout`, and `global-width` and gives
   the next step an accurate accounting of emitted bytes. Test positive and
   negative addends, pointer fields at unaligned offsets, 32- and 64-bit
   relocations, neighboring literal bytes, and references to other globals.
   Preserve the existing deferred handling of mutually referring symbols.

5. **Model alignment using actual section positions.**

   Replace `curAlign` as the sole representation of alignment with explicit
   data/alignment fragments and section-position tracking. Finalize padding
   from the current section offset, requested alignment, fill value and width,
   and maximum bytes to emit. Distinguish alignment of a symbol from alignment
   directives inside its contents.

   Do not reset the section position at every label. Preserve state across
   section switches. Do not guess offsets when code, aliases, or relaxation
   make the layout unresolved: obtain the layout from LLVM's assembler, or
   reject that form until it is modeled. Avoid turning this fix into a general
   object-file lifter rewrite.

   Validate against bytes and relocation offsets emitted by `llvm-mc`, not
   just LLVM IR produced by the lifter. Cover already-aligned positions,
   interior padding, nonzero fill, maximum-padding suppression, multiple
   objects, and section switches. Retain QEMU execution of `global-align`.

6. **Give HI20/LO12 relocations real register semantics.**

   Resolve the symbol and addend, compute the actual HI20/LO12 relocation
   fields, and execute the instruction semantics on those fields. Symbolic
   LUI must write its sign-extended 32-bit result; ADDI, loads, and stores must
   use the current base register plus the sign-extended low immediate.
   Prefer this executable arithmetic model to matching nearby instruction
   pairs: register copies, control-flow joins, and intervening clobbers should
   work through ordinary data flow.

   Prototype the interaction with Alive2's symbolic global addresses first.
   Correct HI20/LO12 pairs rely on linker representability constraints, while
   unconstrained 64-bit global addresses need not fit the relocation. Model
   genuine link-layout constraints explicitly or use a justified conservative
   abstraction. Do not add function-input assumptions merely to make the
   verification succeed, choose arbitrary fixed global addresses, or simplify
   a pair to a full pointer without establishing the necessary conditions.

   Start with the absolute relocations currently accepted. Keep PC-relative,
   GOT, TLS, and relaxation forms explicitly unsupported unless their own
   semantics are implemented. Test low-part carry boundaries around 0x7ff and
   0x800, signed ranges, addends, standalone LUI, mismatched symbols, copied and
   clobbered bases, control-flow joins, and address uses in loads and stores.
   Re-run concrete cases at several legal linker placements; one fixed address
   is not sufficient validation of relocation semantics.

7. **Validate the combined result and document remaining limits.**

   Require all eight original wrong targets to stop receiving successful
   validation, with precise counterexamples for supported forms. Their correct
   counterparts and normal compiler-generated controls must continue to pass.
   Preserve QEMU as an independent check of the expected source/target
   outcomes, and use assembler byte/relocation comparisons for data cases.

   Run the full RISC-V suite. Run the relevant AArch64 tests and full suite for
   changes in the shared data representation. Repeat performance measurements
   after combining the memory and relocation changes. Diagnose new
   unsupported results and timeouts separately from semantic regressions.
   Keep any approved loss of coverage explicit; removing an XFAIL alone does
   not demonstrate restored support.

The main design risks are machine-memory protection's effect on optimization
and solver cost, and relocation arithmetic's interaction with symbolic address
layout. The ABI check and expression/packed-layout changes are more bounded.
Implementing the memory protection before the relocation arithmetic also avoids
exposing new physical memory expressions to the known unsafe optimization path.
