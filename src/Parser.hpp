#pragma once

#include "ASTNode.hpp"
#include "BaseTypeFactory.hpp"
#include "Lexer.hpp"
#include "SymbolTable.hpp"
#include "Type.hpp"

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <unordered_set>
#include <vector>

class Parser {
  public:
    explicit Parser(std::istream & = std::cin);
    ~Parser() = default;

    bool Parse();                    // parse
    const llvm::Module & Generate(); // generate

  private:
    class ErrorLog {
      public:
        ErrorLog(Position pos, std::string msg)
            : _pos(pos), _msg(std::move(msg)) {}
        friend std::ostream & operator<<(std::ostream & os,
                                         const ErrorLog & err) {
            const char * red_fmt = "\033[31m";
            const char * fmt_clear = "\033[m";
            bool in_term = isatty(STDOUT_FILENO);

            if (in_term) {
                os << red_fmt << "[ERROR] " << fmt_clear;
            } else {
                os << "[ERROR] ";
            }
            os << "Near " << err._pos << ": " << err._msg;
            return os;
        }

      private:
        Position _pos;
        std::string _msg;
    };
    Token getNextToken();

    Lexer _lexer; // lexer is used to read tokens

    llvm::LLVMContext MilaContext; // llvm context
    llvm::IRBuilder<> MilaBuilder; // llvm builder
    llvm::Module MilaModule;       // llvm module

    std::shared_ptr<SymbolTable> _st;
    std::vector<ErrorLog> _err;
    std::shared_ptr<ASTNode> _current_code;
    std::unordered_set<std::string> _forward_declared;
    BaseTypeFactory _tf;
    std::unordered_set<std::string> _builtin_names;

    void llvm_init_lib();

    ast_ptr Factor();
    ast_ptr Expression();
    ast_ptr Unary();
    ast_ptr Mul();
    ast_ptr Add();
    ast_ptr Const();
    ASTNodeConst::ConstExpr Const_declaration();
    ast_ptr Var();
    ast_ptr Block();
    VariableRecord Var_declaration();
    std::list<VariableRecord> Var_decl_list();
    ast_ptr Function();
    std::list<VariableRecord> Function_arg();
    Type * Var_type();
    std::list<ast_ptr> Statement();
    ast_ptr Stmt_helper();
    ast_ptr Procedure();
    ast_ptr If();
    ast_ptr Body();
    ast_ptr While();
    ast_ptr For();
    ast_ptr Call(const Token &);
    ast_ptr Mila();
    ast_ptr VarByRef();
    std::shared_ptr<ASTNodeAssignable> ArrayAccess(const std::string &);
    std::pair<int, int> ArrayBounds();

    static bool is_mul_operator(TokenType);
    static bool is_add_operator(TokenType);
    static bool is_rel_operator(TokenType);
    static bool is_statement(TokenType);
};