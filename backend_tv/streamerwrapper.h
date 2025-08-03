#pragma once

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/MCTargetOptionsCommandFlags.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"

namespace lifter {
class MCStreamerWrapper;
}

#include "backend_tv/lifter.h"
#include "backend_tv/mc2llvm.h"

namespace lifter {

// Represents a basic block of machine instructions
class MCBasicBlock {
private:
  std::string name;
  std::vector<llvm::MCInst> Instrs;
  llvm::SetVector<MCBasicBlock *> Succs;

public:
  MCBasicBlock(const std::string &name) : name(name) {}

  const std::string &getName() const {
    return name;
  }

  auto &getInstrs() {
    return Instrs;
  }

  bool empty() const {
    return Instrs.size() == 0;
  }

  auto &getSuccs() {
    return Succs;
  }

  void addInst(llvm::MCInst &inst) {
    Instrs.push_back(inst);
  }

  void addInstBegin(llvm::MCInst &&inst) {
    Instrs.insert(Instrs.begin(), std::move(inst));
  }

  void addSucc(MCBasicBlock *succ_block) {
    Succs.insert(succ_block);
  }
};

struct OffsetSym {
  std::string sym;
  long offset;
};

typedef std::variant<OffsetSym, char> RODataItem;

struct MCGlobal {
  std::string name;
  llvm::Align align;
  std::string section;
  std::vector<RODataItem> data;
};

class MCFunction {
  std::string name;
  unsigned label_cnt{0};

public:
  std::vector<MCBasicBlock> BBs;
  std::vector<MCGlobal> MCglobals;

  MCFunction() {
    MCGlobal g{
        .name = "__stack_chk_guard",
        .align = llvm::Align(8),
        .section = ".rodata",
        // FIXME -- use symbolic data here? does this matter?
        .data = {'7', '7', '7', '7', '7', '7', '7', '7'},
    };
    MCglobals.push_back(g);
  }

  void setName(const std::string &_name) {
    name = _name;
  }

  MCBasicBlock *addBlock(const std::string &b_name) {
    return &BBs.emplace_back(b_name);
  }

  std::string getName() {
    return name;
  }

  std::string getLabel() {
    return name + std::to_string(++label_cnt);
  }

  MCBasicBlock *findBlockByName(const std::string &b_name) {
    for (auto &bb : BBs)
      if (bb.getName() == b_name)
        return &bb;
    return nullptr;
  }

  void checkEntryBlock(unsigned);
};

/*
 * We're overriding MCStreamerWrapper to generate an MCFunction from
 * assembly. MCStreamerWrapper provides callbacks to handle different
 * parts of the assembly file. The callbacks that we're using right
 * now are all emit* functions.
 */
class MCStreamerWrapper final : public llvm::MCStreamer {
  enum ASMLine { none = 0, label = 1, non_term_instr = 2, terminator = 3 };

private:
  const llvm::MCInstrAnalysis &IA;
  MCBasicBlock *curBB{nullptr};
  unsigned prev_line{0};
  llvm::Align curAlign;
  std::string curSym;
  std::string curSec;
  bool FunctionEnded = false;
  unsigned curDebugLine = 0;
  std::vector<RODataItem> curROData;
  unsigned SentinelNOP;
  std::unordered_map<unsigned, llvm::Instruction *> &lineMap;
  std::ostream *out;

public:
  MCFunction MF;
  unsigned cnt{0};

  MCStreamerWrapper(llvm::MCContext &Context, const llvm::MCInstrAnalysis &IA,
                    llvm::MCInstPrinter &InstPrinter, llvm::MCRegisterInfo &MRI,
                    unsigned SentinelNOP,
                    std::unordered_map<unsigned, llvm::Instruction *> &lineMap,
                    std::ostream *out)
      : MCStreamer{Context}, IA{IA}, SentinelNOP{SentinelNOP}, lineMap{lineMap},
        out{out} {}

  void addConstant() {
    if (curROData.empty())
      return;

    MCGlobal g{
        .name = curSym,
        .align = curAlign,
        .section = curSec,
        .data = curROData,
    };
    MF.MCglobals.emplace_back(g);
    curROData.clear();
  }

  // We only want to intercept the emission of new instructions.
  virtual void
  emitInstruction(const llvm::MCInst &Inst,
                  const llvm::MCSubtargetInfo & /* unused */) override;

  std::string attrName(llvm::MCSymbolAttr A) {
    switch (A) {
    case llvm::MCSA_ELF_TypeFunction:
      return "ELF function";
    case llvm::MCSA_ELF_TypeObject:
      return "ELF object";
    case llvm::MCSA_Global:
      return "global";
    default:
      assert(false && "unknown symbol attribute");
    }
  }

  virtual bool emitSymbolAttribute(llvm::MCSymbol *Symbol,
                                   llvm::MCSymbolAttr Attribute) override {
    *out << "[emitSymbolAttribute '" << Symbol->getName().str() << "']\n";
    if (false) {
      *out << "  Common? " << Symbol->isCommon() << "\n";
      *out << "  Variable? " << Symbol->isVariable() << "\n";
    }
    return true;
  }

  virtual void emitSymbolDesc(llvm::MCSymbol *Symbol,
                              unsigned DescValue) override {
    *out << "[emitSymbolDesc '" << Symbol->getName().str() << "']\n";
  }

  virtual void emitCommonSymbol(llvm::MCSymbol *Symbol, uint64_t Size,
                                llvm::Align ByteAlignment) override;

  virtual void emitBytes(llvm::StringRef Data) override;

  virtual void emitFill(const llvm::MCExpr &NumBytes, uint64_t FillValue,
                        llvm::SMLoc Loc) override;

  virtual void emitZerofill(llvm::MCSection *Section,
                            llvm::MCSymbol *Symbol = nullptr, uint64_t Size = 0,
                            llvm::Align ByteAlignment = llvm::Align(1),
                            llvm::SMLoc Loc = llvm::SMLoc()) override {
    *out << "[emitZerofill " << Size << " bytes]\n";
  }

  virtual void emitELFSize(llvm::MCSymbol *Symbol,
                           const llvm::MCExpr *Value) override {
    *out << "[emitELFSize '" << Symbol->getName().str() << "']\n";
    addConstant();
  }

  // FIXME -- we probably need a proper recursive descent parser for
  // MCExprs here
  virtual void emitValueImpl(const llvm::MCExpr *Value, unsigned Size,
                             llvm::SMLoc Loc = llvm::SMLoc()) override;

  virtual void emitValueToAlignment(llvm::Align Alignment, int64_t Fill = 0,
                                    uint8_t FillLen = 1,
                                    unsigned MaxBytesToEmit = 0) override {
    *out << "[emitValueToAlignment= " << Alignment.value() << "]\n";
    curAlign = Alignment;
  }

  virtual void emitAssignment(llvm::MCSymbol *Symbol,
                              const llvm::MCExpr *Value) override {
    *out << "[emitAssignment]\n";
  }

  virtual void emitDwarfLocDirective(unsigned FileNo, unsigned Line,
                                     unsigned Column, unsigned Flags,
                                     unsigned Isa, unsigned Discriminator,
                                     llvm::StringRef FileName,
                                     llvm::StringRef Comment) override {
    *out << "[dwarf loc directive: line = " << Line << "]\n";
    curDebugLine = Line;
  }

  virtual void emitLabel(llvm::MCSymbol *Symbol, llvm::SMLoc Loc) override;

  std::string findTargetLabel(llvm::MCInst &Inst) {
    auto num_operands = Inst.getNumOperands();
    for (unsigned i = 0; i < num_operands; ++i) {
      auto op = Inst.getOperand(i);
      if (op.isExpr()) {
        auto expr = op.getExpr();
        if (expr->getKind() == llvm::MCExpr::ExprKind::SymbolRef) {
          const llvm::MCSymbolRefExpr &SRE = cast<llvm::MCSymbolRefExpr>(*expr);
          const llvm::MCSymbol &Sym = SRE.getSymbol();
          return Sym.getName().str();
        }
      }
    }
    assert(false && "could not find target label in arm branch instruction");
  }

  // Make sure that we have an entry label with no predecessors
  void checkEntryBlock(unsigned jumpOpcode) {
    MF.checkEntryBlock(jumpOpcode);
  }

  // Fill in the CFG
  void generateSuccessors();

  // Remove empty basic blocks, including .Lfunc_end
  void removeEmptyBlocks() {
    erase_if(MF.BBs, [](MCBasicBlock bb) { return bb.empty(); });
  }
};

} // end namespace lifter
