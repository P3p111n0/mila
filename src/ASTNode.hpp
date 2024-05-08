#pragma once

#include "Type.hpp"
#include "ValMap.hpp"
#include "VariableRecord.hpp"
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
#include <variant>

struct CodegenData {
    std::shared_ptr<ValMap<llvm::AllocaInst *>> vars;
    std::shared_ptr<ValMap<llvm::Value *>> consts;
    std::stack<llvm::BasicBlock *> break_addrs;
    std::stack<llvm::BasicBlock *> cont_addrs;
};

class ASTNodeInt;
class ASTNodeIdentifier;
class ASTNodeUnary;
class ASTNodeBinary;
class ASTNodeBody;
class ASTNodeAssign;
class ASTNodeExit;
class ASTNodeIf;
class ASTNodeWhile;
class ASTNodeFor;
class ASTNodeBreak;
class ASTNodeCall;
class ASTNodeVar;
class ASTNodeConst;
class ASTNodePrototype;
class ASTNodeFunction;
class ASTNodeBlock;
class ASTNodeVarByRef;
class ASTNodeTypeCast;
class ASTNodeFBinary;

using ASTVariant =
    std::variant<ASTNodeInt *, ASTNodeIdentifier *, ASTNodeUnary *,
                 ASTNodeBinary *, ASTNodeBody *, ASTNodeAssign *, ASTNodeExit *,
                 ASTNodeIf *, ASTNodeWhile *, ASTNodeFor *, ASTNodeBreak *,
                 ASTNodeCall *, ASTNodeVar *, ASTNodeConst *,
                 ASTNodePrototype *, ASTNodeFunction *, ASTNodeBlock *,
                 ASTNodeVarByRef *, ASTNodeTypeCast *, ASTNodeFBinary *>;

class ASTNode {
  public:
    ASTNode() = default;
    virtual ~ASTNode() = default;
    virtual llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                                  llvm::LLVMContext &, CodegenData &) = 0;
    virtual ASTVariant as_variant() = 0;
};

class ASTNodeInt : public ASTNode {
  public:
    ASTNodeInt(int val) : val(val) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    int val;
};

class ASTNodeIdentifier : public ASTNode {
  public:
    ASTNodeIdentifier(std::string name) : name(std::move(name)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::string name;
};

class ASTNodeUnary : public ASTNode {
  public:
    enum class Operator { Not };

    ASTNodeUnary(ASTNode * arg, Operator op) : arg(arg), op(op) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::shared_ptr<ASTNode> arg;
    Operator op;
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
        : lhs(lhs), rhs(rhs), op(op) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::shared_ptr<ASTNode> lhs;
    std::shared_ptr<ASTNode> rhs;
    Operator op;
};

class ASTNodeFBinary : public ASTNode {
  public:
    enum class Operator { Add, Sub, Mul, Div };
    ASTNodeFBinary(ASTNode * lhs, ASTNode * rhs, Operator op)
        : lhs(lhs), rhs(rhs), op(op) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::shared_ptr<ASTNode> lhs;
    std::shared_ptr<ASTNode> rhs;
    Operator op;
};

class ASTNodeBody : public ASTNode {
  public:
    ASTNodeBody(std::list<std::shared_ptr<ASTNode>> statements)
        : stmts(std::move(statements)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::list<std::shared_ptr<ASTNode>> stmts;
};

class ASTNodeAssign : public ASTNode {
  public:
    ASTNodeAssign(std::string target, ASTNode * rhs)
        : target(std::move(target)), rhs(rhs) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::string target;
    std::shared_ptr<ASTNode> rhs;
};

class ASTNodeExit : public ASTNode {
  public:
    ASTNodeExit() = default;
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
};

class ASTNodeIf : public ASTNode {
  public:
    ASTNodeIf(ASTNode * cond, ASTNode * body)
        : cond(cond), body(body), else_(std::nullopt) {}
    ASTNodeIf(ASTNode * cond, ASTNode * body, ASTNode * else_b)
        : cond(cond), body(body), else_(std::shared_ptr<ASTNode>(else_b)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::shared_ptr<ASTNode> cond;
    std::shared_ptr<ASTNode> body;
    std::optional<std::shared_ptr<ASTNode>> else_;
};

class ASTNodeWhile : public ASTNode {
  public:
    ASTNodeWhile(ASTNode * cond, ASTNode * body) : cond(cond), body(body) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::shared_ptr<ASTNode> cond;
    std::shared_ptr<ASTNode> body;
};

class ASTNodeFor : public ASTNode {
  public:
    ASTNodeFor(std::string var, ASTNode * start, ASTNode * stop, ASTNode * body,
               bool is_downto)
        : var(std::move(var)), it_start(start), it_stop(stop), body(body),
          is_downto(is_downto) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::string var;
    std::shared_ptr<ASTNode> it_start;
    std::shared_ptr<ASTNode> it_stop;
    std::shared_ptr<ASTNode> body;
    bool is_downto;
};

class ASTNodeBreak : public ASTNode {
  public:
    ASTNodeBreak() = default;
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
};

class ASTNodeCall : public ASTNode {
  public:
    ASTNodeCall(std::string fun, std::list<std::shared_ptr<ASTNode>> args)
        : fn(std::move(fun)), args(std::move(args)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::string fn;
    std::list<std::shared_ptr<ASTNode>> args;
};

class ASTNodeVar : public ASTNode {
  public:
    ASTNodeVar(std::list<VariableRecord> variables)
        : vars(std::move(variables)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::list<VariableRecord> vars;
};

class ASTNodeConst : public ASTNode {
  public:
    struct ConstExpr {
        ConstExpr(std::string const_name, std::shared_ptr<ASTNode> expr)
            : name(std::move(const_name)), value(expr) {}
        std::string name;
        std::shared_ptr<ASTNode> value;
    };
    ASTNodeConst(std::list<ConstExpr> c) : constants(std::move(c)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::list<ConstExpr> constants;
};

class ASTNodePrototype : public ASTNode {
  public:
    ASTNodePrototype(std::string name, std::list<VariableRecord> arguments,
                     std::shared_ptr<Type> return_type)
        : fn_name(std::move(name)), args(std::move(arguments)),
          arity(args.size()), return_type(std::move(return_type)) {}

    const std::string & name() const { return fn_name; }
    llvm::Function * codegen(llvm::Module &, llvm::IRBuilder<> &,
                             llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::string fn_name;
    std::list<VariableRecord> args;
    std::size_t arity;
    std::shared_ptr<Type> return_type;
};

class ASTNodeFunction : public ASTNode {
  public:
    ASTNodeFunction(ASTNodePrototype * prototype, ASTNode * block,
                    ASTNode * body)
        : proto(prototype), block(block), body(body) {}
    llvm::Function * codegen(llvm::Module &, llvm::IRBuilder<> &,
                             llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::shared_ptr<ASTNodePrototype> proto;
    std::shared_ptr<ASTNode> block;
    std::shared_ptr<ASTNode> body;
};

class ASTNodeBlock : public ASTNode {
  public:
    ASTNodeBlock(std::list<std::shared_ptr<ASTNode>> declarations)
        : decls(std::move(declarations)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::list<std::shared_ptr<ASTNode>> decls;
};

class ASTNodeVarByRef : public ASTNode {
  public:
    ASTNodeVarByRef(std::string var) : var(std::move(var)) {}
    llvm::AllocaInst * codegen(llvm::Module &, llvm::IRBuilder<> &,
                               llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::string var;
};

class ASTNodeTypeCast : public ASTNode {
  public:
    ASTNodeTypeCast(Type * src, Type * dst, ASTNode * arg)
        : src(src), dst(dst), arg(arg) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }

    std::shared_ptr<Type> src;
    std::shared_ptr<Type> dst;
    std::shared_ptr<ASTNode> arg;
};