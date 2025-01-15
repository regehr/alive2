#include <iostream>

#include "aslp_bridge.h"
#include "../../lifter.h"

#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/MCTargetOptionsCommandFlags.h"


llvm::Function* liftFunc() {


  std::string targerror;
  auto Targ = llvm::TargetRegistry::lookupTarget(lifter::TripleName, targerror);
  std::unique_ptr<llvm::MCInstrInfo> MCII(Targ->createMCInstrInfo());
  assert(MCII && "Unable to create instruction info!");

  llvm::Triple TheTriple(lifter::TripleName);

  // llvm::MCTargetOptions MCOptions = llvm::mc::InitMCTargetOptionsFromFlags();
  llvm::MCTargetOptions MCOptions;
  std::unique_ptr<llvm::MCRegisterInfo> MRI(Targ->createMCRegInfo(lifter::TripleName));
  assert(MRI && "Unable to create target register info!");

  std::unique_ptr<llvm::MCSubtargetInfo> STI(Targ->createMCSubtargetInfo(lifter::TripleName, lifter::CPU, ""));
  assert(STI && "Unable to create subtarget info!");
  assert(STI->isCPUStringValid(lifter::CPU) && "Invalid CPU!");

  std::unique_ptr<llvm::MCAsmInfo> MAI(Targ->createMCAsmInfo(*MRI, lifter::TripleName, MCOptions));
  assert(MAI && "Unable to create MC asm info!");
  std::unique_ptr<llvm::MCInstPrinter> IP(
      Targ->createMCInstPrinter(TheTriple, 0, *MAI, *MCII, *MRI));
  IP->setPrintImmHex(true);

  auto Ana = std::make_unique<llvm::MCInstrAnalysis>(MCII.get());

  llvm::MCContext Ctx(TheTriple, MAI.get(), MRI.get(), STI.get(), &SrcMgr, &MCOptions);
  std::unique_ptr<llvm::MCObjectFileInfo> MCOFI(Targ->createMCObjectFileInfo(Ctx, false, false));
  Ctx.setObjectFileInfo(MCOFI.get());

  // llvm::mc::MCStreamerWrapper Str(Ctx, Ana.get(), IP.get(), MRI.get());
  // Str.setUseAssemblerInfoForParsing(true);
  auto m = std::make_unique<llvm::Module>("tempModule");

  std::unique_ptr<llvm::MCCodeEmitter> MCE{Targ->createMCCodeEmitter(*MCII.get(), Ctx)};
  assert(MCE && "createMCCodeEmitter failed.");

  lifter::MCFunction* MF;

  lifter::arm2llvm arm2llvm{LiftedModule, *MF, nullptr, IP.get(), *MCE, *STI, *Ana};

  std::string sss;
  llvm::raw_string_ostream ss(sss);
  if (llvm::verifyModule(*LiftedModule, &ss)) {
    *out << sss << "\n\n";
    out->flush();
    *out << "\nERROR: Lifted module is broken, this should not happen\n";
    exit(-1);
  }

  return make_pair(srcFn, liftedFn);

int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "usage: " << argv[0] << " ASLT_FILE\n";
    return 1;
  }
  long& a = reinterpret_cast<long&>(empty);


  aslp::bridge bridge{lifter_interface_llvm& iface, const llvm::MCCodeEmitter& mce, const llvm::MCSubtargetInfo& sti, const llvm::MCInstrAnalysis& ia)

  return 0;
}
