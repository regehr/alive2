# aslp-bridge

This is the [Aslp](https://github.com/UQ-PAC/aslp) semantics provider for the arm-tv tool.
This has some dependencies in addition to those from Alive2 and arm-tv (e.g. LLVM at head with RTTI+EH).

Requirements:
- ANTLR4 parser framework, installed by your system package manager.
- [aslp-cpp](https://github.com/UQ-PAC/aslp/tree/partial_eval/aslp-cpp), which should be fetched automatically.
If needed, these can be given explicitly as cmake arguments. Here is an example:
```bash
cmake -B build -DBUILD_TV=1 \
  -DCMAKE_PREFIX_PATH=./antlr-dev/.';'~/progs/llvm-regehr/llvm/build/ \
  -DANTLR4_JAR_LOCATION=./antlr-4.13.0-complete.jar \
  -DFETCHCONTENT_SOURCE_DIR_ASLP-CPP=~/progs/aslp 
```

You will also need the aslp-server which provides the Aslp semantics over HTTP.
The suggested way to get this is using the Nix package manager. Once this is installed, use
```bash
nix --extra-experimental-features nix-command --extra-experimental-features flakes shell github:katrinafyi/pac-nix#aslp --command aslp-server 
```
This should build and launch aslp-server.
Otherwise, you can compile with Dune from the [aslp](https://github.com/UQ-PAC/aslp) repository then run `dune exec aslp-server`.

## usage

In this fork, the Aslp integration is enabled by default in the backend-tv executable.
When running, you should see some additional output when preparing the instructions and
in the main lifted function, blocks will be named with aslp\_.
*Important!* Make sure aslp-server is running, otherwise backend-tv will hang at "Waiting for server to start".

The behaviour of the Aslp bridge can be configured by environment variables:
- ASLP (default: true) enables or disables the entire Aslp functionality,
- ASLP\_DEBUG (default: false) enables debug logging when traversing the Aslp syntax tree,
- ASLP\_BANNED (default: "") is a comma-separated list of MCInst integer opcodes to prevent Aslp from processing,
- ASLP\_SERVER (default: "localhost:8000") is the address and port where aslp-server is running,

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


fish shell:
```fish
cmake -GNinja -B build -DBUILD_TV=1 -DCMAKE_PREFIX_PATH=./antlr-dev/. -DLLVM_DIR=~/progs/llvm-regehr/llvm/build/lib/cmake/llvm -DANTLR4_JAR_LOCATION=(cat antlr-jar) -DFETCHCONTENT_SOURCE_DIR_ASLP-CPP=~/progs/aslp && cmake --build build -j10 -t backend-tv
```
