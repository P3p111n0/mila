#include "ASTNode.hpp"
#include <stack>


using namespace llvm;

static std::stack<BasicBlock*> _break_addr;

Type * resolve_type(LLVMContext & ctx, VarType t) {
    switch (t) {
    case VarType::Int:
        return Type::getInt32Ty(ctx);
    case VarType::Void:
        return Type::getVoidTy(ctx);
    default:
        return nullptr;
    }
}

static AllocaInst * CreateEntryBlockAlloca(Function * f, Type * type,
                                           const std::string & name) {
    IRBuilder<> TmpB(&f->getEntryBlock(), f->getEntryBlock().begin());
    return TmpB.CreateAlloca(type, nullptr, name);
}

Value * ASTNodeInt::codegen(llvm::Module &, llvm::IRBuilder<> &,
                            llvm::LLVMContext & ctx,
                            std::map<std::string, llvm::AllocaInst *> &) {
    return ConstantInt::get(ctx, APInt(32, _val, true));
}

Value *
ASTNodeIdentifier::codegen(llvm::Module &, llvm::IRBuilder<> & builder,
                           llvm::LLVMContext &,
                           std::map<std::string, llvm::AllocaInst *> & st) {
    AllocaInst * val = st[_name];
    if (!val) {
        //TODO error
    }
    return builder.CreateLoad(val->getAllocatedType(), val, _name);
}

Value * ASTNodeUnary::codegen(llvm::Module & module,
                              llvm::IRBuilder<> & builder,
                              llvm::LLVMContext & ctx,
                              std::map<std::string, llvm::AllocaInst *> & st) {
    Value * arg = _arg->codegen(module, builder, ctx, st);
    switch (_op) {
    case Operator::Not:
        return builder.CreateNot(arg, "not_tmp");
    default:
        return nullptr;
    }
}

Value * ASTNodeBinary::codegen(llvm::Module & module,
                               llvm::IRBuilder<> & builder,
                               llvm::LLVMContext & ctx,
                               std::map<std::string, llvm::AllocaInst *> & st) {
    Value * lhs = _lhs->codegen(module, builder, ctx, st);
    Value * rhs = _rhs->codegen(module, builder, ctx, st);

    switch (_op) {
    case Operator::Add:
        return builder.CreateAdd(lhs, rhs, "add_tmp");
    case Operator::Sub:
        return builder.CreateSub(lhs, rhs, "sub_tmp");
    case Operator::Mul:
        return builder.CreateMul(lhs, rhs, "mul_tmp");
    case Operator::Div:
        return builder.CreateSDiv(lhs, rhs, "div_tmp");
    case Operator::Or:
        return builder.CreateOr(lhs, rhs, "or_tmp");
    case Operator::Xor:
        return builder.CreateXor(lhs, rhs, "xor_tmp");
    case Operator::And:
        return builder.CreateAnd(lhs, rhs, "and_tmp");
    case Operator::Mod:
        return builder.CreateURem(lhs, rhs, "mod_tmp");
    case Operator::Eq:
        return builder.CreateICmpEQ(lhs, rhs, "eq_tmp");
    case Operator::NEq:
        return builder.CreateICmpNE(lhs, rhs, "neq_tmp");
    case Operator::Lt:
        return builder.CreateICmpSLT(lhs, rhs, "lt_tmp");
    case Operator::Gt:
        return builder.CreateICmpSGT(lhs, rhs, "gt_tmp");
    case Operator::LtE:
        return builder.CreateICmpSLE(lhs, rhs, "lte_tmp");
    case Operator::GtE:
        return builder.CreateICmpSGE(lhs, rhs, "gte_tmp");
    default:
        return nullptr;
    }
}

Function *
ASTNodePrototype::codegen(Module & module, IRBuilder<> &,
                          LLVMContext & ctx,
                          std::map<std::string, llvm::AllocaInst *> &) {
    std::vector<llvm::Type *> arg_types(_args.size(),
                                        llvm::Type::getInt32Ty(ctx));

    llvm::Type * ret_type = resolve_type(ctx, _return_type);

    FunctionType * ft = FunctionType::get(ret_type, arg_types, false);
    Function * f =
        Function::Create(ft, Function::ExternalLinkage, _fn_name, module);

    auto it = _args.begin();
    for (auto & arg : f->args()) {
        arg.setName(it->name);
    }
    return f;
}

Function *
ASTNodeFunction::codegen(Module & module, IRBuilder<> & builder,
                         LLVMContext & ctx,
                         std::map<std::string, llvm::AllocaInst *> & st) {
    Function * function = module.getFunction(_proto->name());

    if (!function) {
        function = _proto->codegen(module, builder, ctx, st);
    }

    if (!function) {
        return nullptr;
    }

    if (!function->empty()) {
        // TODO error
    }

    BasicBlock * entry = BasicBlock::Create(ctx, "entry", function);
    builder.SetInsertPoint(entry);

    std::map<std::string, AllocaInst*> old_st = std::move(st);
    st = {};
    // insert return value var
    if (function->getReturnType()->getTypeID() != Type::VoidTyID) {
        AllocaInst * ret_var =
            CreateEntryBlockAlloca(function, function->getReturnType(),
                                   std::string(function->getName()));
        builder.CreateStore(Constant::getNullValue(ret_var->getAllocatedType()), ret_var);
        st[std::string(function->getName())] = ret_var;
    }

    for (auto & arg : function->args()) {
        AllocaInst * alloca = CreateEntryBlockAlloca(
            function, arg.getType(), std::string(arg.getName()));
        builder.CreateStore(Constant::getNullValue(alloca->getAllocatedType()), alloca);
        st[std::string(arg.getName())] = alloca;
    }

    _block->codegen(module, builder, ctx, st);

    // TODO body
    builder.SetInsertPoint(entry);
    BasicBlock * function_body = BasicBlock::Create(ctx, "function_body", function);
    builder.CreateBr(function_body);
    builder.SetInsertPoint(function_body);
    _body->codegen(module, builder, ctx, st);

    Value * ret_val = nullptr;
    if (function->getReturnType()->getTypeID() != Type::VoidTyID) {
        // load return value if expected
        ret_val = builder.CreateLoad(function->getReturnType(),
                                     st[std::string(function->getName())],
                                     "return_value");
    }

    builder.CreateRet(ret_val);
    verifyFunction(*function);
    //restore symbol table
    st = std::move(old_st);
    return function;
}

Value * ASTNodeCall::codegen(Module & module, IRBuilder<> & builder,
                             LLVMContext & ctx,
                             std::map<std::string, llvm::AllocaInst *> & st) {
    Function * function = module.getFunction(_fn);
    if (!function) {
        //TODO error
    }

    std::vector<Value*> args;
    for (auto & arg : _args) {
        args.push_back(arg->codegen(module, builder, ctx, st));
        //TODO compile error handling
    }
    return builder.CreateCall(function, args, "call_tmp");
}

Value * ASTNodeIf::codegen(Module & module, IRBuilder<> & builder,
                             LLVMContext & ctx,
                             std::map<std::string, llvm::AllocaInst *> & st) {
    Value * cond = _cond->codegen(module, builder, ctx, st);
    Value * null = Constant::getNullValue(cond->getType());
    cond = builder.CreateICmpNE(cond, null, "if_cond");

    Function * function = builder.GetInsertBlock()->getParent();
    BasicBlock * then_bb = BasicBlock::Create(ctx, "then_block", function);
    BasicBlock * else_bb = BasicBlock::Create(ctx, "else_block", function);
    BasicBlock * cont = BasicBlock::Create(ctx, "after_block", function);
    builder.CreateCondBr(cond, then_bb, else_bb);

    //then branch
    builder.SetInsertPoint(then_bb);
    // TODO body
    _body->codegen(module, builder, ctx, st);
    builder.CreateBr(cont);

    //else branch
    function->insert(function->end(), else_bb);
    builder.SetInsertPoint(else_bb);
    //TODO body
    if (_else.has_value()) {
        _else.value()->codegen(module, builder, ctx, st);
    }
    builder.CreateBr(cont);

    //after if block
    function->insert(function->end(), cont);
    builder.SetInsertPoint(cont);
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

Value * ASTNodeBody::codegen(Module & module, IRBuilder<> & builder,
                             LLVMContext & ctx,
                             std::map<std::string, llvm::AllocaInst *> & st) {
    for (auto & stmt : _stmts) {
        stmt->codegen(module, builder, ctx, st);
        // TODO error handling
    }
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

Value * ASTNodeExit::codegen(Module &, IRBuilder<> & builder,
                             LLVMContext &,
                             std::map<std::string, llvm::AllocaInst *> & st) {
    Function * function = builder.GetInsertBlock()->getParent();
    Value * ret = nullptr;
    if (function->getReturnType()->getTypeID() != Type::VoidTyID) {
        AllocaInst * ret_alloca = st[function->getName().str()];
        ret = builder.CreateLoad(function->getReturnType(), ret_alloca, "return_value");
    }
    return builder.CreateRet(ret);
}

Value * ASTNodeFor::codegen(Module & module, IRBuilder<> & builder,
                             LLVMContext & ctx,
                             std::map<std::string, llvm::AllocaInst *> & st) {
    Value * init_val = _it_start->codegen(module, builder, ctx, st);
    //TODO compile error handling

    Function * function = builder.GetInsertBlock()->getParent();
    AllocaInst * loop_var = nullptr;
    if (!st.count(_var)) {
        loop_var =
            CreateEntryBlockAlloca(function, Type::getInt32Ty(ctx), _var);
    } else {
        loop_var = st[_var];
    }
    builder.CreateStore(init_val, loop_var);

    // main loop body
    BasicBlock * loop_bb = BasicBlock::Create(ctx, "loop", function);
    BasicBlock * loop_end = BasicBlock::Create(ctx, "loop_end");
    _break_addr.push(loop_end);
    builder.CreateBr(loop_bb);
    builder.SetInsertPoint(loop_bb);
    _body->codegen(module, builder, ctx, st);
    Value * step = ConstantInt::get(ctx, APInt(32, 1, true));
    Value * end_cond = _it_stop->codegen(module, builder, ctx, st);

    // mutate iter var
    Value * current_val = builder.CreateLoad(Type::getInt32Ty(ctx), loop_var);
    Value * next_val = nullptr;
    if (_is_downto) {
        next_val = builder.CreateSub(current_val, step, "step");
    } else {
        next_val = builder.CreateAdd(current_val, step, "step");
    }
    builder.CreateStore(next_val, loop_var);
    end_cond = builder.CreateICmpEQ(next_val, end_cond);
    function->insert(function->end(), loop_end);
    builder.CreateCondBr(end_cond, loop_end, loop_bb);
    builder.SetInsertPoint(loop_end);
    _break_addr.pop();
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

Value * ASTNodeAssign::codegen(Module & module, IRBuilder<> & builder,
                             LLVMContext & ctx,
                             std::map<std::string, llvm::AllocaInst *> & st) {
    AllocaInst * var = st[_target];
    Value * value = _rhs->codegen(module, builder, ctx, st);
    builder.CreateStore(value, var);
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

Value * ASTNodeBreak::codegen(Module &, IRBuilder<> & builder,
                               LLVMContext & ctx,
                               std::map<std::string, llvm::AllocaInst *> &) {
    if (_break_addr.empty()) {
        //TODO error
    }
    BasicBlock * loop_end = _break_addr.top();
    builder.CreateBr(loop_end);
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

Value * ASTNodeVar::codegen(Module &, IRBuilder<> & builder,
                              LLVMContext & ctx,
                              std::map<std::string, llvm::AllocaInst *> & st) {
    Function * function = builder.GetInsertBlock()->getParent();
    for (const auto & var : _vars) {
        Type * type = resolve_type(ctx, var.type);
        AllocaInst * alloca = CreateEntryBlockAlloca(function, type, var.name);
        builder.CreateStore(Constant::getNullValue(alloca->getAllocatedType()), alloca);
        st[var.name] = alloca;
    }
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

Value * ASTNodeConst::codegen(Module & module, IRBuilder<> & builder,
                            LLVMContext & ctx,
                            std::map<std::string, llvm::AllocaInst *> & st) {
    Function * function = builder.GetInsertBlock()->getParent();
    for (const auto & c : _constants) {
        Type * type = resolve_type(ctx, VarType::Int);
        AllocaInst * alloca = CreateEntryBlockAlloca(function, type, c.name);
        Value * val = c.value->codegen(module, builder, ctx, st);
        builder.CreateStore(val, alloca);
        st[c.name] = alloca;
    }
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

Value * ASTNodeBlock::codegen(Module & module, IRBuilder<> & builder,
                              LLVMContext & ctx,
                              std::map<std::string, llvm::AllocaInst *> & st) {
    for (auto & block : _decls) {
        block->codegen(module, builder, ctx, st);
    }
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

Value * ASTNodeWhile::codegen(Module & module, IRBuilder<> & builder,
                              LLVMContext & ctx,
                              std::map<std::string, llvm::AllocaInst *> & st) {
    Function * function = builder.GetInsertBlock()->getParent();
    BasicBlock * loop_cond = BasicBlock::Create(ctx, "while_cond", function);
    BasicBlock * loop_body = BasicBlock::Create(ctx, "while_body", function);
    BasicBlock * loop_end = BasicBlock::Create(ctx, "while_end");
    _break_addr.push(loop_end);

    builder.CreateBr(loop_cond);
    builder.SetInsertPoint(loop_cond);
    Value * cond = _cond->codegen(module, builder, ctx, st);
    Value * null = Constant::getNullValue(cond->getType());
    Value * end_cond = builder.CreateICmpNE(cond, null, "end_cond");
    builder.CreateCondBr(end_cond, loop_body, loop_end);

    builder.SetInsertPoint(loop_body);
    _body->codegen(module, builder, ctx, st); // TODO error handling
    builder.CreateBr(loop_cond);

    function->insert(function->end(), loop_end);
    builder.SetInsertPoint(loop_end);
    return Constant::getNullValue(Type::getVoidTy(ctx));
}