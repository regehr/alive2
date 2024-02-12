#include <cassert>
#include <iostream>
#include <filesystem>
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

llvm::LLVMContext* Context;

namespace aslp {

bridge::bridge(lifter_interface& iface, const llvm::MCCodeEmitter& mce, const llvm::MCSubtargetInfo& sti, const llvm::MCInstrAnalysis& ia) 
  : iface{iface}, mce{mce}, sti{sti}, ia{ia} { }


std::optional<bridge::parsed_t> bridge::parse(const std::filesystem::path& path) const& {
  std::ifstream file{path};
  if (file.fail()) {
    return std::nullopt;
  }

  antlr4::ANTLRInputStream input{file};
  aslt::SemanticsLexer lexer{&input};
  antlr4::CommonTokenStream tokens{&lexer};
  aslt::SemanticsParser parser{&tokens};

  std::unique_ptr<llvm::Module> Mod = std::make_unique<llvm::Module>("", *Context);
  assert(Mod);
  auto funty = llvm::FunctionType::get(llvm::Type::getVoidTy(*Context), {}, false);
  auto fun = llvm::Function::Create(funty, llvm::GlobalValue::LinkageTypes::ExternalLinkage, "aslp_entry", Mod.get());
  auto *bb = llvm::BasicBlock::Create(*Context, "", fun);

  aslt_visitor visitor{iface};

  auto ctx = parser.stmts();
  assert(ctx && "parsing failed! syntax error?");
  return ctx ? std::make_optional(std::ref(*ctx)) : std::nullopt;
}


stmt_t bridge::visit(const bridge::parsed_t ctx) const& {
  aslt_visitor visitor{iface};
  return std::any_cast<stmt_t>(visitor.visitStmts(&ctx.get()));
}

#ifndef ASLT_DIR
#define ASLT_DIR "./aslt"
#endif


std::variant<err_t, stmt_t> bridge::run(const llvm::MCInst& inst, const opcode_t& bytes) const& {
  if (ia.isBranch(inst)) {
    return err_t::banned;
  }


  std::string opstr = std::format("0x{:08x}", get_opnum(bytes));

  fs::path aslt_path{ASLT_DIR};
  aslt_path /= opstr + ".aslt";
  aslt_path = std::filesystem::absolute(aslt_path);

  auto parsed = parse(aslt_path);
  if (!parsed) return err_t::missing;

  auto ret = visit(parsed.value());

  std::cerr << "ASLP FINISHED!" << std::endl;
  return ret;
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

