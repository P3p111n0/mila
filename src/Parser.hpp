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

#include "Lexer.hpp"
#include "ASTNode.hpp"
#include "SymbolTable.hpp"

class Parser {
  public:
    Parser();
    ~Parser() = default;

    bool Parse();                    // parse
    const llvm::Module & Generate(); // generate

  private:
    Token getNextToken();

    Lexer _lexer; // lexer is used to read tokens

    llvm::LLVMContext MilaContext; // llvm context
    llvm::IRBuilder<> MilaBuilder; // llvm builder
    llvm::Module MilaModule;       // llvm module

    std::shared_ptr<SymbolTable> _st;

    ASTNode * Factor();
    ASTNode * Expression();
    ASTNode * Unary();
    ASTNode * Mul();
    ASTNode * Add();
    void Const();
    void Const_recursive();
    void Var();
    void Var_optional();
    void Var_recursive();
    VariableRecord Var_declaration();
    void Function();
    std::list<VariableRecord> Function_arg();
    Type Var_type();
    std::list<std::shared_ptr<ASTNode>> Statement();
    ASTNode * Stmt_helper();
    ASTNode * Assignment();
    void Procedure();
    ASTNode * If();
    ASTNode * Body();

    static bool is_mul_operator(TokenType);
    static bool is_add_operator(TokenType);
    static bool is_rel_operator(TokenType);
    static bool is_statement(TokenType);
};

#endif // PJPPROJECT_PARSER_HPP
