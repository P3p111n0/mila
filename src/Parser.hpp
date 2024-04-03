#ifndef PJPPROJECT_PARSER_HPP
#define PJPPROJECT_PARSER_HPP

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

#include "ASTNode.hpp"
#include "Lexer.hpp"
#include "SymbolTable.hpp"
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

    void llvm_init_lib();

    ASTNode * Factor();
    ASTNode * Expression();
    ASTNode * Unary();
    ASTNode * Mul();
    ASTNode * Add();
    ASTNode * Const();
    ASTNodeConst::ConstExpr Const_declaration();
    ASTNode * Var();
    ASTNode * Block();
    VariableRecord Var_declaration();
    ASTNode * Function();
    std::list<VariableRecord> Function_arg();
    VarType Var_type();
    std::list<std::shared_ptr<ASTNode>> Statement();
    ASTNode * Stmt_helper();
    ASTNode * Procedure();
    ASTNode * If();
    ASTNode * Body();
    ASTNode * While();
    ASTNode * For();
    ASTNode * Call(const Token &);
    ASTNode * Mila();

    static bool is_mul_operator(TokenType);
    static bool is_add_operator(TokenType);
    static bool is_rel_operator(TokenType);
    static bool is_statement(TokenType);
};

#endif // PJPPROJECT_PARSER_HPP
