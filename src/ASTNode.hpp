#pragma once

#include "VariableRecord.hpp"
#include "ValMap.hpp"
#include "Type.hpp"
#include <list>
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
#include <llvm/IR/ValueSymbolTable.h>
#include <llvm/IR/Verifier.h>
#include <map>
#include <memory>
#include <optional>
#include <stack>

struct CodegenData {
    std::shared_ptr<ValMap<llvm::AllocaInst*>> vars;
    std::shared_ptr<ValMap<llvm::Value*>> consts;
    std::stack<llvm::BasicBlock *> break_addrs;
    std::stack<llvm::BasicBlock *> cont_addrs;
};

class ASTNode {
  public:
    ASTNode() = default;
    virtual ~ASTNode() = default;
    virtual llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                                  llvm::LLVMContext &, CodegenData &) = 0;
};

class ASTNodeInt : public ASTNode {
  public:
    ASTNodeInt(int val) : _val(val) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    int _val;
};

class ASTNodeIdentifier : public ASTNode {
  public:
    ASTNodeIdentifier(std::string name) : _name(std::move(name)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    std::string _name;
};

class ASTNodeUnary : public ASTNode {
  public:
    enum class Operator { Not };

    ASTNodeUnary(ASTNode * arg, Operator op) : _arg(arg), _op(op) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    std::shared_ptr<ASTNode> _arg;
    Operator _op;
};

class ASTNodeBinary : public ASTNode {
  public:
    enum class Operator {
        Add,
        Sub,
        Mul,
        Div,
        Or,
        Xor,
        And,
        Mod,
        Eq,
        NEq,
        Lt,
        Gt,
        LtE,
        GtE
    };

    ASTNodeBinary(ASTNode * lhs, ASTNode * rhs, Operator op)
        : _lhs(lhs), _rhs(rhs), _op(op) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    std::shared_ptr<ASTNode> _lhs;
    std::shared_ptr<ASTNode> _rhs;
    Operator _op;
};

class ASTNodeBody : public ASTNode {
  public:
    ASTNodeBody(std::list<std::shared_ptr<ASTNode>> statements)
        : _stmts(std::move(statements)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    std::list<std::shared_ptr<ASTNode>> _stmts;
};

class ASTNodeAssign : public ASTNode {
  public:
    ASTNodeAssign(std::string target, ASTNode * rhs)
        : _target(std::move(target)), _rhs(rhs) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    std::string _target;
    std::shared_ptr<ASTNode> _rhs;
};

class ASTNodeExit : public ASTNode {
  public:
    ASTNodeExit() = default;
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
};

class ASTNodeIf : public ASTNode {
  public:
    ASTNodeIf(ASTNode * cond, ASTNode * body)
        : _cond(cond), _body(body), _else(std::nullopt) {}
    ASTNodeIf(ASTNode * cond, ASTNode * body, ASTNode * else_b)
        : _cond(cond), _body(body), _else(std::shared_ptr<ASTNode>(else_b)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    std::shared_ptr<ASTNode> _cond;
    std::shared_ptr<ASTNode> _body;
    std::optional<std::shared_ptr<ASTNode>> _else;
};

class ASTNodeWhile : public ASTNode {
  public:
    ASTNodeWhile(ASTNode * cond, ASTNode * body) : _cond(cond), _body(body) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    std::shared_ptr<ASTNode> _cond;
    std::shared_ptr<ASTNode> _body;
};

class ASTNodeFor : public ASTNode {
  public:
    ASTNodeFor(std::string var, ASTNode * start, ASTNode * stop, ASTNode * body,
               bool is_downto)
        : _var(std::move(var)), _it_start(start), _it_stop(stop), _body(body),
          _is_downto(is_downto) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    std::string _var;
    std::shared_ptr<ASTNode> _it_start;
    std::shared_ptr<ASTNode> _it_stop;
    std::shared_ptr<ASTNode> _body;
    bool _is_downto;
};

class ASTNodeBreak : public ASTNode {
  public:
    ASTNodeBreak() = default;
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
};

class ASTNodeCall : public ASTNode {
  public:
    ASTNodeCall(std::string fun, std::list<std::shared_ptr<ASTNode>> args)
        : _fn(std::move(fun)), _args(std::move(args)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    std::string _fn;
    std::list<std::shared_ptr<ASTNode>> _args;
};

class ASTNodeVar : public ASTNode {
  public:
    ASTNodeVar(std::list<VariableRecord> variables)
        : _vars(std::move(variables)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    std::list<VariableRecord> _vars;
};

class ASTNodeConst : public ASTNode {
  public:
    struct ConstExpr {
        ConstExpr(std::string const_name, std::shared_ptr<ASTNode> expr)
            : name(std::move(const_name)), value(expr) {}
        std::string name;
        std::shared_ptr<ASTNode> value;
    };
    ASTNodeConst(std::list<ConstExpr> c) : _constants(std::move(c)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    std::list<ConstExpr> _constants;
};

class ASTNodePrototype : public ASTNode {
  public:
    ASTNodePrototype(std::string name, std::list<VariableRecord> arguments,
                     std::shared_ptr<Type> return_type)
        : _fn_name(std::move(name)), _args(std::move(arguments)),
          _arity(_args.size()), _return_type(std::move(return_type)) {}

    const std::string & name() const { return _fn_name; }
    llvm::Function * codegen(llvm::Module &, llvm::IRBuilder<> &,
                             llvm::LLVMContext &, CodegenData &) override;

  private:
    std::string _fn_name;
    std::list<VariableRecord> _args;
    std::size_t _arity;
    std::shared_ptr<Type> _return_type;
};

class ASTNodeFunction : public ASTNode {
  public:
    ASTNodeFunction(ASTNodePrototype * prototype, ASTNode * block,
                    ASTNode * body)
        : _proto(prototype), _block(block), _body(body) {}
    llvm::Function * codegen(llvm::Module &, llvm::IRBuilder<> &,
                             llvm::LLVMContext &, CodegenData &) override;

  private:
    std::shared_ptr<ASTNodePrototype> _proto;
    std::shared_ptr<ASTNode> _block;
    std::shared_ptr<ASTNode> _body;
};

class ASTNodeBlock : public ASTNode {
  public:
    ASTNodeBlock(std::list<std::shared_ptr<ASTNode>> declarations)
        : _decls(std::move(declarations)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;

  private:
    std::list<std::shared_ptr<ASTNode>> _decls;
};

class ASTNodeVarByRef : public ASTNode {
  public:
    ASTNodeVarByRef(std::string var) : _var(std::move(var)) {}
    llvm::AllocaInst * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
  private:
    std::string _var;
};

class ASTNodeTypeCast : public ASTNode {
  public:
    ASTNodeTypeCast(Type * src, Type * dst) : _src(src), _dst(dst) {}
  private:
    std::shared_ptr<Type> _src;
    std::shared_ptr<Type> _dst;
};