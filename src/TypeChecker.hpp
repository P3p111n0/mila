#pragma once

#include "ASTNode.hpp"
#include "SymbolTable.hpp"
#include "Type.hpp"
#include "BaseTypeFactory.hpp"
#include <memory>
#include <vector>
#include <ostream>

struct TypeResult {
    ASTNode * node;
    type_ptr type;
};

class TypeChecker {
  public:
    TypeChecker(std::shared_ptr<SymbolTable> symbol_table)
        : _st(std::move(symbol_table)) {}

    void errs(std::ostream &) const;

    std::shared_ptr<ASTNode> tree_rebuild(ASTNode *);

    TypeResult operator()(ASTNodeInt *);
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
    TypeResult operator()(ASTNode *);

  private:
    class TypeError {
      public:
        TypeError(std::string msg)
            : _msg(std::move(msg)) {}
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
