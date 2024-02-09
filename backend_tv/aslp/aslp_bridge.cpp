#include <cassert>
#include <iostream>
#include <filesystem>

#include <antlr4-runtime.h>
#include <llvm/ADT/ArrayRef.h>
#include "ANTLRInputStream.h"
#include "CommonTokenStream.h"
#include "SemanticsParser.h"
#include "SemanticsLexer.h"

#include "SemanticsVisitor.h"
#include "aslp/interface.hpp"
#include "aslt_visitor.hpp"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/GlobalValue.h"

namespace fs = std::filesystem;

llvm::LLVMContext* Context;

namespace aslp {

static_assert(!std::is_abstract<aslt_visitor>(), "aslt_visitor must not be abstract");

stmt_t parse(const fs::path& path, lifter_interface& iface) {
  std::ifstream file{path};

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

  return std::any_cast<stmt_t>(visitor.visitStmts(parser.stmts()));
}

#ifndef ASLT_DIR
#define ASLT_DIR "./aslt"
#endif

stmt_t run(lifter_interface& iface) {
  fs::path aslt_dir{ASLT_DIR};
  fs::path aslt_path{fs::absolute(aslt_dir / "adds.aslt")};

  assert(fs::exists(aslt_path) && "aslt does not exist at");
  std::cout << "aslt: " << aslt_path << '\n';

  Context = &iface.ll_function().getContext();
  auto ret = aslp::parse(aslt_path, iface);

  std::cerr << "ASLP FINISHED!" << std::endl;
  return ret;
}

} // namespace aslp

