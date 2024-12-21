# aslp-bridge

([compare ahead](https://github.com/regehr/alive2/compare/arm-tv...katrinafyi:alive2:aslp))

This is the [Aslp](https://github.com/UQ-PAC/aslp) semantics provider for the arm-tv tool.
Given an LLVM MCInst, it consults Aslp for partially-evaluated semantics then translates
its Aslt format into LLVM IR compatible with the existing lifter.

Requirements:
- very recent LLVM (tested with 20.0.0git at https://github.com/llvm/llvm-project/commit/35710ab392b50c815765f03c12409147502dfb86), built with RTTI+EH.
- ANTLR4 parser framework.
- [aslp-cpp](https://github.com/UQ-PAC/aslp/tree/partial_eval/aslp-cpp), which should be fetched automatically.
- [aslp-server](https://github.com/UQ-PAC/aslp), running alongside the backend-tv tool, see below.

## building

These instructions will install all of the required dependencies and build the backend-tv tool.

1. Install the Nix package manager:
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
   ```
2. Allow your user to use custom Nix caches for faster installation:
   ```bash
   printf '%s\n' "extra-trusted-users = $USER" | sudo tee -a /etc/nix/nix.conf
   ```
3. Install dependencies and build the backend-tv tool:
   ```bash
   ./build.sh  # say 'y' if prompted for substituters or trusted keys
   ```
4. Start aslp-server and leave running:
   ```bash
   ./build/aslp/bin/aslp-server  # or otherwise, if already installed
   ```
5. Use backend-tv, for example:
   ```bash
   ./build/backend-tv ./tests/arm-tv/cmp/sgt.aarch64.ll
   ```

If desired, the tool can also be built manually by adapting the following cmake command.
This might be useful to use local versions of particular dependencies.
```bash
cmake -B build -DBUILD_TV=1 \
  -DCMAKE_PREFIX_PATH=./antlr-dev/.';'~/progs/llvm-regehr/llvm/build/ \
  -DANTLR4_JAR_LOCATION=./antlr-4.13.0-complete.jar \
  -DFETCHCONTENT_SOURCE_DIR_ASLP-CPP=~/progs/aslp 
```

<!--
You will also need the *aslp-server* which provides the Aslp semantics over HTTP.
The suggested way to get this is using the Nix package manager. Once Nix is installed, use
```bash
nix --extra-experimental-features nix-command --extra-experimental-features flakes shell github:katrinafyi/pac-nix#aslp --command aslp-server 
```
This should build and launch aslp-server from the [pac-nix](https://github.com/katrinafyi/pac-nix) packages.
Otherwise, you can compile with Dune from the [aslp](https://github.com/UQ-PAC/aslp) repository then run `dune exec aslp-server`.
-->

## usage

In this fork, the Aslp integration is enabled by default in the backend-tv executable.
When running, you should see some additional output when preparing the instructions and
in the main lifted function, blocks will be named with aslp\_.

**Important!** Make sure aslp-server is running, otherwise backend-tv will hang at "Waiting for server to start".

The behaviour of the Aslp bridge can be configured by environment variables:
- ASLP (default: true) enables or disables the entire Aslp functionality,
- ASLP\_DEBUG (default: false) enables debug logging when traversing the Aslp syntax tree,
- ASLP\_BANNED (default: "") is a comma-separated list of MCInst integer opcodes to prevent Aslp from processing,
- ASLP\_SERVER (default: "localhost:8000") is the address and port where aslp-server is running, and
- ASLP\_VECTORS (default: true) controls whether Aslp should provide vector operations where possible.

To compare with the original ("classic") lifter, there is a script ./diff.sh in the repository.
This takes a single test file as argument and runs both the Aslp and classic variants.
The output from each, and the diff between them, will be written into separate files.
For example,
```bash-session
$ ./diff.sh ./tests/arm-tv/vectors/ucvtf/UCVTFUWSri.aarch64.ll
[ ... ]
finished! backend-tv './tests/arm-tv/vectors/ucvtf/UCVTFUWSri.aarch64.ll'
  aslp (status 0): /home/rina/progs/alive2-regehr/out/UCVTFUWSri.aarch64.aslp.ll
  classic (status 0): /home/rina/progs/alive2-regehr/out/UCVTFUWSri.aarch64.aslp.ll
  diff: /home/rina/progs/alive2-regehr/out/UCVTFUWSri.aarch64.ll.diff
```

To process the output of `lit.py -s` and identify test cases which differ
between aslp and classic, one can use:
```bash
python3 -c '[print(x) for x in open("/dev/stdin").read().split("\n\n") if "\n" not in x]' < tests.txt
```

## structure

The Aslp-specific files are placed in this folder. In a roughly bottom-up order,
- The ANTLR grammar in *Semantics.g4* describes the Aslt textual semantics format. This is processed by ANTLR into a lexer, parser, and stubs for visitors.
- We define an *aslt\_visitor* which traverses this syntax tree and generates LLVM.
  This visits in a top-down order. Methods such as expr(), stmt(), lexpr() are used to recurse into subexpressions of particular types.
  The header defines type aliases (suffixed by "_t") which are the conventional LLVM translations of each syntactic structure.
  To start the visitor, the aslt\_visitor::visitStmts method is called with a parser producing a list of statements.
- The *aslp\_bridge* files are the entry point to the Aslp side from lifter.cpp.
  This reads environment variables and determines whether MCInsts are suitable for Aslp.
  It constructs the parser and lexer, calls the aslt\_visitor, then returns the status of the translation.
- In *lifter.cpp*, code is added to the beginning of arm2llvm::liftInst.
  If appropriate, this calls the aslp\_bridge.
  This code also has the task of using LLVM's assembler to encode an MCInst into opcode bytes.
- The *arm2llvm* class is used from within the aslt\_visitor to manage the basic blocks and create LLVM instructions.
  This is done by implementing an abstract interface, declared in interface.h.

## state
Currently (2024-02-19), the Aslp-based lifter has fairly good outcomes with the arm-tv test suite (on i5-13600H).
>  Expected Passes    : 6661 <br/>
>  Unsupported Tests  : 9<br/>
>  Unexpected Failures: 8<br/>
>  Individual Timeouts: 268<br/>

For the classic lifter (done by setting ASLP=0 before running lit.py),
>  Expected Passes    : 6735<br/>
>  Unsupported Tests  : 9<br/>
>  Unexpected Failures: 3<br/>
>  Individual Timeouts: 199<br/>

The Aslp-failing tests (with the first 3 also failing in classic) are:
```
Alive2 :: arm-tv/bugs/load_i1.aarch64.ll
Alive2 :: arm-tv/vectors/bugs/srem.aarch64.ll
Alive2 :: arm-tv/stack/STRXui_1.aarch64.ll

Alive2 :: arm-tv/calls/vec.aarch64.ll
Alive2 :: arm-tv/fp/fneg/fneg-2.aarch64.ll
Alive2 :: arm-tv/instructions/MRS-2.aarch64.ll
Alive2 :: arm-tv/instructions/MSR-1.aarch64.ll
Alive2 :: arm-tv/intrinsics/trap.aarch64.ll
```
Of these, vec fails due to different vector poison behaviour, fneg fails due to storing pointers,
and MRS/MSR/trap are not yet implemented in the aslt\_visitor.

## details

The Aslp approach, while promising, is complicated to implement due to the differences in abstraction between LLVM MCInst and the ARM opcode
which Aslp uses.
MCInst is a fairly high level and convenient representation, with symbolic references to blocks and symbols.
This information is lost during the lowering to ARM opcodes.

The MCInst representation also enables better handling of pointers, since the semantics for a pointer-using instruction
can be written with this in mind.
LLVM's pointer semantics are highly sensitive and accidental integer/pointer mixing can cause undefined behaviour.
In Aslp, the visitor does not have this context ahead-of-time.
For example, when a value is used as a pointer with an offset, we must backtrack and undo its integer addition,
replacing these with getelementptr instructions.
This works in the simple cases but has limitations.
One fix (hypothetically) is storing two versions of every variable (i.e. local variable or register),
one as a regular bitvector and another as a pointer, and accessing the correct one depending on the use.

Vector operations are another difference between the two lifters.
The classic lifter, operating in LLVM, takes advantage of LLVM's vector functionality where possible.
This makes the semantics cleaner and easier to reason about.
Aslp, however, treats vector registers no different from ordinary scalar registers and performs vector operations by
bitwise operations on the full 128-bit width.
This is obviously much worse for Alive to reason with and, as in the calls/vec test, affects how poison values spread across vector elements. 
It is conceivable that an LLVM optimisation pass could detect and replace these operations with their vector counterparts,
but it does not do so right now.

Floating-point operations are also limited, specifically by ignoring the FPSR and FPCR (status and control, resp.) registers.

Edit 2024-12-17: During this year, improvements have been made to both ASLp (by adding specialised vector operations and floating-point intrinsics)
and arm-tv (by memory model improvements) which make address these shortcomings.
