#include "SemanticsBaseVisitor.h"
#include "aslt_visitor.hpp"

using namespace aslt;




// std::any aslp_visitor::visitStmt(SemanticsParser::StmtContext *context) {
//   std::cout << "stmt ";
//   return std::string{"booped"};
// }

std::any aslt_visitor::visitStmts(SemanticsParser::StmtsContext *ctx) {
  std::cout << "stmts ";
  return SemanticsBaseVisitor::visitStmts(ctx);
}
