# ARM-TV TODO

- ensure that when we do the ARM side of an i1 store, only 0 or 1
  is stored
- rig llvm-reduce to be a fuzzer
- refactor so instructions go into separate files and use a proper API
- support jump tables, requires symbolic register contents
- anytime SP is updated, assert that it's a multiple of 16
- we're making a mess out of handling layoutinfo and targettriple
  - let's use the included info by default
  - but have a flag to force this, for processing bitcode without ARM target info
- assert that stack memory is not trashed
  - saved ret addr
  - saved FP
  - what else?
- stop using instrs_64 and friends, just use the register numbers
  in the instructions to get the correct-sized operands
- support function calls
- size the stack correctly, instead of hard-coding a size
  - https://llvm.org/doxygen/classllvm_1_1AArch64FunctionInfo.html
  - https://llvm.org/docs/StackMaps.html
- write a script that finds test cases for instructions where we lack a test
- find a good set of opt passes that doesn't trigger middle-end FP bugs
  - then turn FP back on and keep adding instructions
- mark parameters noundef?
  - probably not necessary since we're turning on Alive2's flags to
    disable poison and undef inputs

- more vector instructions
- more memory operations
- more FP operations
