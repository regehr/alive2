#include <iostream>

#include "aslp_bridge.h"
#include "../../lifter.h"

#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/MCTargetOptionsCommandFlags.h"

int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "usage: " << argv[0] << " ASLT_FILE\n";
    return 1;
  }

  return 0;
}
