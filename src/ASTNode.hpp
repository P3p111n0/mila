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

struct MemoryLocation {
    llvm::Value * ptr;
    llvm::Type * pointee_type;
};

struct CodegenData {
    CodegenData() = default;
    std::shared_ptr<ValMap<MemoryLocation>> vars;
    std::shared_ptr<ValMap<llvm::Value *>> consts;
    std::stack<llvm::BasicBlock *> break_addrs;
    std::stack<llvm::BasicBlock *> cont_addrs;
};

class ASTNode;
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
class ASTNodeBuiltinCall;
class ASTNodeString;
class ASTNodeArrAccess;
class ASTNodeDouble;
class ASTNodeContinue;

using ast_ptr = std::shared_ptr<ASTNode>;

using ASTVariant = std::variant<
    ASTNodeInt *, ASTNodeIdentifier *, ASTNodeUnary *, ASTNodeBinary *,
    ASTNodeBody *, ASTNodeAssign *, ASTNodeExit *, ASTNodeIf *, ASTNodeWhile *,
    ASTNodeFor *, ASTNodeBreak *, ASTNodeCall *, ASTNodeVar *, ASTNodeConst *,
    ASTNodePrototype *, ASTNodeFunction *, ASTNodeBlock *, ASTNodeVarByRef *,
    ASTNodeTypeCast *, ASTNodeFBinary *, ASTNodeBuiltinCall *, ASTNodeString *,
    ASTNodeArrAccess *, ASTNodeDouble *, ASTNodeContinue *>;

class ASTNode {
  public:
    ASTNode() = default;
    virtual ~ASTNode() = default;
    virtual llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                                  llvm::LLVMContext &, CodegenData &) = 0;
    virtual ASTVariant as_variant() = 0;
    virtual ASTNode * shallow_copy() const = 0;
};

class ASTNodeInt : public ASTNode {
  public:
    ASTNodeInt(int val) : val(val) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeInt * shallow_copy() const override { return new ASTNodeInt(*this); }

    int val;
};

class ASTNodeDouble : public ASTNode {
  public:
    ASTNodeDouble(double val) : val(val) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeDouble * shallow_copy() const override {
        return new ASTNodeDouble(*this);
    }

    double val;
};

class ASTNodeAssignable : public ASTNode {
  public:
    ASTNodeAssignable(std::string name) : name(std::move(name)) {}
    virtual llvm::Value * get_allocated_ptr(llvm::Module &, llvm::IRBuilder<> &,
                                            llvm::LLVMContext &,
                                            CodegenData &) const = 0;

    std::string name;
};

class ASTNodeIdentifier : public ASTNodeAssignable {
  public:
    ASTNodeIdentifier(std::string name) : ASTNodeAssignable(name) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    llvm::Value * get_allocated_ptr(llvm::Module &, llvm::IRBuilder<> &,
                                    llvm::LLVMContext &,
                                    CodegenData &) const override;
    ASTVariant as_variant() override { return this; }
    ASTNodeIdentifier * shallow_copy() const override {
        return new ASTNodeIdentifier(*this);
    }
};

class ASTNodeArrAccess : public ASTNodeAssignable {
  public:
    ASTNodeArrAccess(std::string name, std::vector<ast_ptr> index_list)
        : ASTNodeAssignable(std::move(name)), idx_list(std::move(index_list)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    llvm::Value * get_allocated_ptr(llvm::Module &, llvm::IRBuilder<> &,
                                    llvm::LLVMContext &,
                                    CodegenData &) const override;
    ASTVariant as_variant() override { return this; }
    ASTNodeArrAccess * shallow_copy() const override {
        return new ASTNodeArrAccess(*this);
    }

    std::vector<ast_ptr> idx_list;
};

class ASTNodeUnary : public ASTNode {
  public:
    enum class Operator { Not };

    ASTNodeUnary(ast_ptr arg, Operator op) : arg(arg), op(op) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeUnary * shallow_copy() const override {
        return new ASTNodeUnary(*this);
    }

    ast_ptr arg;
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

    ASTNodeBinary(ast_ptr lhs, ast_ptr rhs, Operator op)
        : lhs(lhs), rhs(rhs), op(op) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeBinary * shallow_copy() const override {
        return new ASTNodeBinary(*this);
    }

    ast_ptr lhs;
    ast_ptr rhs;
    Operator op;
};

class ASTNodeFBinary : public ASTNode {
  public:
    enum class Operator { Add, Sub, Mul, Div, Mod, Eq, NEq, Lt, Gt, LtE, GtE };
    ASTNodeFBinary(ast_ptr lhs, ast_ptr rhs, Operator op)
        : lhs(lhs), rhs(rhs), op(op) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeFBinary * shallow_copy() const override {
        return new ASTNodeFBinary(*this);
    }

    ast_ptr lhs;
    ast_ptr rhs;
    Operator op;
};

class ASTNodeBody : public ASTNode {
  public:
    ASTNodeBody(std::list<ast_ptr> statements) : stmts(std::move(statements)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeBody * shallow_copy() const override {
        return new ASTNodeBody(*this);
    }

    std::list<ast_ptr> stmts;
};

class ASTNodeAssign : public ASTNode {
  public:
    ASTNodeAssign(std::shared_ptr<ASTNodeAssignable> target, ast_ptr rhs)
        : target(std::move(target)), rhs(rhs) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeAssign * shallow_copy() const override {
        return new ASTNodeAssign(*this);
    }

    std::shared_ptr<ASTNodeAssignable> target;
    ast_ptr rhs;
};

class ASTNodeExit : public ASTNode {
  public:
    ASTNodeExit() = default;
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeExit * shallow_copy() const override { return new ASTNodeExit(); }
};

class ASTNodeIf : public ASTNode {
  public:
    ASTNodeIf(ast_ptr cond, ast_ptr body)
        : cond(cond), body(body), else_(std::nullopt) {}
    ASTNodeIf(ast_ptr cond, ast_ptr body, ast_ptr else_b)
        : cond(cond), body(body), else_(ast_ptr(else_b)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeIf * shallow_copy() const override { return new ASTNodeIf(*this); }

    ast_ptr cond;
    ast_ptr body;
    std::optional<ast_ptr> else_;
};

class ASTNodeWhile : public ASTNode {
  public:
    ASTNodeWhile(ast_ptr cond, ast_ptr body) : cond(cond), body(body) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeWhile * shallow_copy() const override {
        return new ASTNodeWhile(*this);
    }

    ast_ptr cond;
    ast_ptr body;
};

class ASTNodeFor : public ASTNode {
  public:
    ASTNodeFor(std::string var, ast_ptr start, ast_ptr stop, ast_ptr body,
               bool is_downto)
        : var(std::move(var)), it_start(start), it_stop(stop), body(body),
          is_downto(is_downto) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeFor * shallow_copy() const override { return new ASTNodeFor(*this); }

    std::string var;
    ast_ptr it_start;
    ast_ptr it_stop;
    ast_ptr body;
    bool is_downto;
};

class ASTNodeBreak : public ASTNode {
  public:
    ASTNodeBreak() = default;
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeBreak * shallow_copy() const override { return new ASTNodeBreak(); }
};

class ASTNodeContinue : public ASTNode {
  public:
    ASTNodeContinue() = default;
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeContinue * shallow_copy() const override {
        return new ASTNodeContinue();
    }
};

class ASTNodeCall : public ASTNode {
  public:
    ASTNodeCall(std::string fun, std::list<ast_ptr> args)
        : fn(std::move(fun)), args(std::move(args)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeCall * shallow_copy() const override {
        return new ASTNodeCall(*this);
    }

    std::string fn;
    std::list<ast_ptr> args;
};

class ASTNodeBuiltinCall : public ASTNodeCall {
  public:
  public:
    ASTNodeBuiltinCall(std::string fun, std::list<ast_ptr> args)
        : ASTNodeCall(std::move(fun), std::move(args)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeBuiltinCall * shallow_copy() const override {
        return new ASTNodeBuiltinCall(*this);
    }
};

class ASTNodeVar : public ASTNode {
  public:
    ASTNodeVar(std::list<VariableRecord> variables)
        : vars(std::move(variables)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeVar * shallow_copy() const override { return new ASTNodeVar(*this); }

    std::list<VariableRecord> vars;
};

class ASTNodeConst : public ASTNode {
  public:
    struct ConstExpr {
        ConstExpr(std::string const_name, ast_ptr expr)
            : name(std::move(const_name)), value(expr) {}
        std::string name;
        ast_ptr value;
    };
    ASTNodeConst(std::list<ConstExpr> c) : constants(std::move(c)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeConst * shallow_copy() const override {
        return new ASTNodeConst(*this);
    }

    std::list<ConstExpr> constants;
};

class ASTNodePrototype : public ASTNode {
  public:
    ASTNodePrototype(std::string name, std::list<VariableRecord> arguments,
                     std::shared_ptr<Type> return_type, bool forward)
        : fn_name(std::move(name)), args(std::move(arguments)),
          arity(args.size()), return_type(std::move(return_type)),
          is_forward_declared(forward) {}

    const std::string & name() const { return fn_name; }
    llvm::Function * codegen(llvm::Module &, llvm::IRBuilder<> &,
                             llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodePrototype * shallow_copy() const override {
        return new ASTNodePrototype(*this);
    }

    std::string fn_name;
    std::list<VariableRecord> args;
    std::size_t arity;
    std::shared_ptr<Type> return_type;
    bool is_forward_declared;
};

class ASTNodeFunction : public ASTNode {
  public:
    ASTNodeFunction(std::shared_ptr<ASTNodePrototype> prototype, ast_ptr block,
                    ast_ptr body)
        : proto(prototype), block(block), body(body) {}
    llvm::Function * codegen(llvm::Module &, llvm::IRBuilder<> &,
                             llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeFunction * shallow_copy() const override {
        return new ASTNodeFunction(*this);
    }

    std::shared_ptr<ASTNodePrototype> proto;
    ast_ptr block;
    ast_ptr body;
};

class ASTNodeBlock : public ASTNode {
  public:
    ASTNodeBlock(std::list<ast_ptr> declarations)
        : decls(std::move(declarations)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeBlock * shallow_copy() const override {
        return new ASTNodeBlock(*this);
    }

    std::list<ast_ptr> decls;
};

class ASTNodeVarByRef : public ASTNode {
  public:
    ASTNodeVarByRef(std::shared_ptr<ASTNodeAssignable> ptr)
        : var(std::move(ptr)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeVarByRef * shallow_copy() const override {
        return new ASTNodeVarByRef(*this);
    }

    std::shared_ptr<ASTNodeAssignable> var;
};

class ASTNodeTypeCast : public ASTNode {
  public:
    ASTNodeTypeCast(type_ptr dst, ast_ptr arg) : dst(dst), arg(arg) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeTypeCast * shallow_copy() const override {
        return new ASTNodeTypeCast(*this);
    }

    std::shared_ptr<Type> dst;
    ast_ptr arg;
};

class ASTNodeString : public ASTNode {
  public:
    ASTNodeString(std::string val) : str(std::move(val)) {}
    llvm::Value * codegen(llvm::Module &, llvm::IRBuilder<> &,
                          llvm::LLVMContext &, CodegenData &) override;
    ASTVariant as_variant() override { return this; }
    ASTNodeString * shallow_copy() const override {
        return new ASTNodeString(*this);
    }

    std::string str;
};