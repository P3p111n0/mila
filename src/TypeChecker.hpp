#pragma once

#include "ASTNode.hpp"
#include "BaseTypeFactory.hpp"
#include "SymbolTable.hpp"
#include "Type.hpp"
#include <memory>
#include <ostream>
#include <vector>

struct TypeResult {
    ast_ptr node;
    type_ptr type;
};

class TypeChecker {
  public:
    TypeChecker(std::shared_ptr<SymbolTable> symbol_table)
        : _st(std::move(symbol_table)) {}

    void errs(std::ostream &) const;

    ast_ptr tree_rebuild(ast_ptr);
    type_ptr get_expr_type(ast_ptr);

    TypeResult operator()(ASTNodeInt *);
    TypeResult operator()(ASTNodeDouble *);
    TypeResult operator()(ASTNodeString *);
    TypeResult operator()(ASTNodeUnary *);
    TypeResult operator()(ASTNodeBinary *);
    TypeResult operator()(ASTNodeFBinary *);
    TypeResult operator()(ASTNodeIdentifier *);
    TypeResult operator()(ASTNodeArrAccess *);
    TypeResult operator()(ASTNodeAssign *);
    TypeResult operator()(ASTNodeCall *);
    TypeResult operator()(ASTNodeBuiltinCall *);
    TypeResult operator()(ASTNodeVarByRef *);
    TypeResult operator()(ASTNodeFunction *);
    TypeResult operator()(ASTNodeIf *);
    TypeResult operator()(ASTNodeWhile *);
    TypeResult operator()(ASTNodeFor *);
    TypeResult operator()(ASTNodeBody *);
    TypeResult operator()(ASTNodeBlock *);
    TypeResult operator()(ASTNodeVar *);
    TypeResult operator()(ASTNode *);

  private:
    class TypeError {
      public:
        TypeError(std::string msg) : _msg(std::move(msg)) {}
        friend std::ostream & operator<<(std::ostream & os,
                                         const TypeError & err) {
            const char * red_fmt = "\033[31m";
            const char * fmt_clear = "\033[m";
            bool in_term = isatty(STDOUT_FILENO);

            if (in_term) {
                os << red_fmt << "[ERROR] " << fmt_clear;
            } else {
                os << "[ERROR] ";
            }
            os << err._msg;
            return os;
        }

      private:
        std::string _msg;
    };

    std::shared_ptr<SymbolTable> _st;
    std::vector<TypeError> _errs;
    BaseTypeFactory _tf;
};
