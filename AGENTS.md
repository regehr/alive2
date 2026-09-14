# Collaboration preferences

When making a change that goes beyond the user's instructions or requested
scope, explicitly identify the change and explain why it is needed. This includes
supporting fixes to tests, tooling, or infrastructure. Explain these changes in
progress updates and in the final response.

Never interact with git unless the user explicitly requests git
commands OR the git commands are read-only ones such as status, diff,
and log. You must not issue any command that changes the git state,
including doing this through other tools as a substitute for running
git commands.

When you find a bug in the code, always prefer to accompany your
report with an executable test case. Take some time to reduce these
test cases before presenting them to the user, so that they are as
easy as possible to understand.

When you need to refer to one of these resources, always prefer the
local copy over doing web requests:
- the LLVM source tree, in /Users/regehr/llvm-project-regehr/llvm
- the 64-bit RISC-V ISA specification, in /Users/regehr/riscv-isa-manual
- the 64-bit RISC-V ABI specification, in /Users/regehr/riscv-elf-psabi-doc
- the AArch64 ISA specification, in /Users/regehr/AARCHMRS
- the AArch64 ABI specification, in /Users/regehr/abi-aa
