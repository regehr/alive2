#include <cassert>
#include <iostream>
#include <filesystem>
#include <memory>
#include <optional>
#include <ranges>

#include <antlr4-runtime.h>
#include <llvm/ADT/ArrayRef.h>
#include "ANTLRInputStream.h"
#include "CommonTokenStream.h"
#include "SemanticsParser.h"
#include "SemanticsLexer.h"

#include "aslp/interface.hpp"
#include "aslt_visitor.hpp"
#include "aslp_bridge.hpp"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/MC/MCInstrAnalysis.h"


namespace fs = std::filesystem;

namespace aslp {

bridge::bridge(lifter_interface& iface, const llvm::MCCodeEmitter& mce, const llvm::MCSubtargetInfo& sti, const llvm::MCInstrAnalysis& ia) 
  : iface{iface}, context{iface.ll_function().getContext()}, mce{mce}, sti{sti}, ia{ia} { }


std::variant<err_t, stmt_t> bridge::parse(const std::filesystem::path& path) {
  std::ifstream file{path};
  if (file.fail()) {
    return err_t::missing;
  }

  antlr4::ANTLRInputStream input{file};
  aslt::SemanticsLexer lexer{&input};
  antlr4::CommonTokenStream tokens{&lexer};

  assert(!parser);
  parser = std::make_unique<aslt::SemanticsParser>(&tokens);

  aslt_visitor visitor{iface};

  auto ctx = parser->stmts();
  assert(ctx && "parsing failed! syntax error?");

  return std::any_cast<stmt_t>(visitor.visitStmts(ctx));
}

#ifndef ASLT_DIR
#define ASLT_DIR "./aslt"
#endif


std::variant<err_t, stmt_t> bridge::run(const llvm::MCInst& inst, const opcode_t& bytes) {
  bool banned = ia.isBranch(inst) || ia.isReturn(inst) || ia.isCall(inst) || ia.isIndirectBranch(inst);
  if (banned)
    return err_t::banned;

  std::string opstr = std::format("0x{:08x}", get_opnum(bytes));

  fs::path aslt_path{ASLT_DIR};
  aslt_path /= opstr + ".aslt";
  aslt_path = std::filesystem::absolute(aslt_path);

  auto parsed = parse(aslt_path);
  auto ret = std::get_if<stmt_t>(&parsed);
  if (!ret)
    return parsed;

  std::cerr << "ASLP FINISHED!" << std::endl;
  return *ret;
}


std::string format_opcode(const opcode_t &bytes) {
  return std::format(
      "{:02x} {:02x} {:02x} {:02x}",
      bytes.at(0), bytes.at(1), bytes.at(2), bytes.at(3));
}

uint32_t get_opnum(const opcode_t &bytes) {
  uint32_t a64opcode = 0;
  for (const auto& x : std::views::reverse(bytes)) {
    a64opcode <<= 8;
    a64opcode |= (0xff & (uint32_t)x);
  }
  return a64opcode;
}

} // namespace aslp

