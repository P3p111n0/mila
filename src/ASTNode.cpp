#include "ASTNode.hpp"
#include <stack>

using namespace llvm;

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
    AllocaInst * alloca = TmpB.CreateAlloca(type, nullptr, name);
    TmpB.CreateStore(Constant::getNullValue(alloca->getAllocatedType()), alloca);
    return alloca;
}

llvm::Value * ASTNodeInt::codegen(llvm::Module &, llvm::IRBuilder<> &,
                                  llvm::LLVMContext & ctx, CodegenData &) {
    return ConstantInt::get(ctx, APInt(32, _val, true));
}

llvm::Value *
ASTNodeIdentifier::codegen(llvm::Module &,
                                         llvm::IRBuilder<> & builder,
                                         llvm::LLVMContext &,
                                         CodegenData & cdg) {
    if (cdg.consts.count(_name)) {
        return cdg.consts[_name];
    } else {
        AllocaInst * val = cdg.vars[_name];
        return builder.CreateLoad(val->getAllocatedType(), val, _name);
    }
}

llvm::Value * ASTNodeUnary::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx, CodegenData & cdg) {
    Value * arg = _arg->codegen(module, builder, ctx, cdg);
    switch (_op) {
    case Operator::Not:
        return builder.CreateNot(arg, "not_tmp");
    default:
        return nullptr;
    }
}

llvm::Value * ASTNodeBinary::codegen(llvm::Module & module,
                                     llvm::IRBuilder<> & builder,
                                     llvm::LLVMContext & ctx,
                                     CodegenData & cdg) {
    Value * lhs = _lhs->codegen(module, builder, ctx, cdg);
    Value * rhs = _rhs->codegen(module, builder, ctx, cdg);

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
                          CodegenData &) {
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
                         CodegenData & cdg) {
    Function * function = module.getFunction(_proto->name());

    if (!function) {
        function = _proto->codegen(module, builder, ctx, cdg);
    }

    if (!function) {
        return nullptr;
    }

    if (!function->empty()) {
        // TODO error
    }

    BasicBlock * entry = BasicBlock::Create(ctx, "entry", function);
    BasicBlock * old_insert_point = builder.GetInsertBlock();
    builder.SetInsertPoint(entry);

    auto old_vars = std::move(cdg.vars);
    auto old_consts = std::move(cdg.consts);
    cdg.vars = {};
    cdg.consts = {};
    // insert return value var
    if (function->getReturnType()->getTypeID() != Type::VoidTyID) {
        AllocaInst * ret_var =
            CreateEntryBlockAlloca(function, function->getReturnType(),
                                   function->getName().str());
        cdg.vars[function->getName().str()] = ret_var;
    }

    for (auto & arg : function->args()) {
        AllocaInst * alloca = CreateEntryBlockAlloca(
            function, arg.getType(), arg.getName().str());
        builder.CreateStore(&arg, alloca);
        cdg.vars[arg.getName().str()] = alloca;
    }

    _block->codegen(module, builder, ctx, cdg);

    // TODO body
    builder.SetInsertPoint(entry);
    BasicBlock * function_body = BasicBlock::Create(ctx, "function_body", function);
    builder.CreateBr(function_body);
    builder.SetInsertPoint(function_body);
    _body->codegen(module, builder, ctx, cdg);

    Value * ret_val = nullptr;
    if (function->getReturnType()->getTypeID() != Type::VoidTyID) {
        // load return value if expected
        ret_val = builder.CreateLoad(function->getReturnType(),
                                     cdg.vars[function->getName().str()],
                                     "return_value");
    }

    builder.CreateRet(ret_val);
    verifyFunction(*function);
    //restore symbol table
    cdg.vars = std::move(old_vars);
    cdg.consts = std::move(old_consts);
    if (old_insert_point) {
        builder.SetInsertPoint(old_insert_point); // restore insertion point
    }
    return function;
}

llvm::Value * ASTNodeCall::codegen(llvm::Module & module,
                                   llvm::IRBuilder<> & builder,
                                   llvm::LLVMContext & ctx, CodegenData & cdg) {
    Function * function = module.getFunction(_fn);
    if (!function) {
        //TODO error
    }

    std::vector<Value*> args;
    for (auto & arg : _args) {
        args.push_back(arg->codegen(module, builder, ctx, cdg));
        //TODO compile error handling
    }
    return builder.CreateCall(function, args, "call_tmp");
}

llvm::Value * ASTNodeIf::codegen(llvm::Module & module,
                                 llvm::IRBuilder<> & builder,
                                 llvm::LLVMContext & ctx, CodegenData & cdg) {
    Value * cond = _cond->codegen(module, builder, ctx, cdg);
    Value * null = Constant::getNullValue(cond->getType());
    cond = builder.CreateICmpNE(cond, null, "if_cond");

    Function * function = builder.GetInsertBlock()->getParent();
    BasicBlock * then_bb = BasicBlock::Create(ctx, "then_block", function);
    BasicBlock * else_bb = BasicBlock::Create(ctx, "else_block");
    BasicBlock * cont = BasicBlock::Create(ctx, "after_block");
    builder.CreateCondBr(cond, then_bb, else_bb);

    //then branch
    builder.SetInsertPoint(then_bb);
    // TODO body
    _body->codegen(module, builder, ctx, cdg);
    builder.CreateBr(cont);

    //else branch
    function->insert(function->end(), else_bb);
    builder.SetInsertPoint(else_bb);
    //TODO body
    if (_else.has_value()) {
        _else.value()->codegen(module, builder, ctx, cdg);
    }
    builder.CreateBr(cont);

    //after if block
    function->insert(function->end(), cont);
    builder.SetInsertPoint(cont);
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeBody::codegen(llvm::Module & module,
                                   llvm::IRBuilder<> & builder,
                                   llvm::LLVMContext & ctx, CodegenData & cdg) {
    for (auto & stmt : _stmts) {
        stmt->codegen(module, builder, ctx, cdg);
        // TODO error handling
    }
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeExit::codegen(llvm::Module &, llvm::IRBuilder<> & builder,
                                   llvm::LLVMContext &, CodegenData & cdg) {
    Function * function = builder.GetInsertBlock()->getParent();
    Value * ret = nullptr;
    if (function->getReturnType()->getTypeID() != Type::VoidTyID) {
        AllocaInst * ret_alloca = cdg.vars[function->getName().str()];
        ret = builder.CreateLoad(function->getReturnType(), ret_alloca, "return_value");
    }
    return builder.CreateRet(ret);
}

llvm::Value * ASTNodeFor::codegen(llvm::Module & module,
                                  llvm::IRBuilder<> & builder,
                                  llvm::LLVMContext & ctx, CodegenData & cdg) {
    Value * init_val = _it_start->codegen(module, builder, ctx, cdg);
    //TODO compile error handling

    Function * function = builder.GetInsertBlock()->getParent();
    AllocaInst * loop_var = nullptr;
    bool loop_var_is_temp = false;
    if (!cdg.vars.count(_var)) {
        loop_var =
            CreateEntryBlockAlloca(function, Type::getInt32Ty(ctx), _var);
        cdg.vars[_var] = loop_var;
        loop_var_is_temp = true;
    } else {
        loop_var = cdg.vars[_var];
    }
    builder.CreateStore(init_val, loop_var, "iter_var_init");

    // main loop body
    BasicBlock * loop_cond = BasicBlock::Create(ctx, "loop_cond", function);
    BasicBlock * loop_bb = BasicBlock::Create(ctx, "loop", function);
    BasicBlock * loop_end = BasicBlock::Create(ctx, "loop_end");
    cdg.break_addrs.push(loop_end);
    cdg.cont_addrs.push(loop_cond);
    builder.CreateBr(loop_cond);
    builder.SetInsertPoint(loop_cond);
    Value * current_val = builder.CreateLoad(Type::getInt32Ty(ctx), loop_var, "loop_current_val");
    Value * end_cond = _it_stop->codegen(module, builder, ctx, cdg);
    Value * loop_cont_cond = builder.CreateICmpNE(current_val, end_cond, "loop_cont_cond");
    builder.CreateCondBr(loop_cont_cond, loop_bb, loop_end);

    builder.SetInsertPoint(loop_bb);
    _body->codegen(module, builder, ctx, cdg);
    Value * step = ConstantInt::get(ctx, APInt(32, 1, true));

    // mutate iter var
    current_val = builder.CreateLoad(Type::getInt32Ty(ctx), loop_var, "loop_var_fetch");
    Value * next_val = nullptr;
    if (_is_downto) {
        next_val = builder.CreateSub(current_val, step, "step");
    } else {
        next_val = builder.CreateAdd(current_val, step, "step");
    }
    builder.CreateStore(next_val, loop_var);
    builder.CreateBr(loop_cond);
    function->insert(function->end(), loop_end);
    builder.SetInsertPoint(loop_end);

    if (loop_var_is_temp) {
        cdg.vars.erase(_var);
    }

    cdg.break_addrs.pop();
    cdg.cont_addrs.pop();
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeAssign::codegen(llvm::Module & module,
                                     llvm::IRBuilder<> & builder,
                                     llvm::LLVMContext & ctx,
                                     CodegenData & cdg) {
    AllocaInst * var = cdg.vars[_target];
    Value * value = _rhs->codegen(module, builder, ctx, cdg);
    builder.CreateStore(value, var);
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeBreak::codegen(llvm::Module &, llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx,
                                    CodegenData & cdg) {
    if (cdg.break_addrs.empty()) {
        //TODO error
    }
    BasicBlock * loop_end = cdg.break_addrs.top();
    builder.CreateBr(loop_end);
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeVar::codegen(llvm::Module &, llvm::IRBuilder<> & builder,
                                  llvm::LLVMContext & ctx, CodegenData & cdg) {
    Function * function = builder.GetInsertBlock()->getParent();
    for (const auto & var : _vars) {
        Type * type = resolve_type(ctx, var.type);
        AllocaInst * alloca = CreateEntryBlockAlloca(function, type, var.name);
        cdg.vars[var.name] = alloca;
    }
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeConst::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx, CodegenData & cdg) {
    Function * function = builder.GetInsertBlock()->getParent();
    BasicBlock * current_block = builder.GetInsertBlock();
    BasicBlock * entry = &function->getEntryBlock();
    builder.SetInsertPoint(entry);
    for (const auto & c : _constants) {
        Value * val = c.value->codegen(module, builder, ctx, cdg);
        cdg.consts[c.name] = val;
    }
    builder.SetInsertPoint(current_block);
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeBlock::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx, CodegenData & cdg) {
    for (auto & block : _decls) {
        block->codegen(module, builder, ctx, cdg);
    }
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeWhile::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx, CodegenData & cdg) {
    Function * function = builder.GetInsertBlock()->getParent();
    BasicBlock * loop_cond = BasicBlock::Create(ctx, "while_cond", function);
    BasicBlock * loop_body = BasicBlock::Create(ctx, "while_body", function);
    BasicBlock * loop_end = BasicBlock::Create(ctx, "while_end");
    cdg.break_addrs.push(loop_end);

    builder.CreateBr(loop_cond);
    builder.SetInsertPoint(loop_cond);
    Value * cond = _cond->codegen(module, builder, ctx, cdg);
    Value * null = Constant::getNullValue(cond->getType());
    Value * end_cond = builder.CreateICmpNE(cond, null, "end_cond");
    builder.CreateCondBr(end_cond, loop_body, loop_end);

    builder.SetInsertPoint(loop_body);
    _body->codegen(module, builder, ctx, cdg); // TODO error handling
    builder.CreateBr(loop_cond);

    function->insert(function->end(), loop_end);
    builder.SetInsertPoint(loop_end);
    return Constant::getNullValue(Type::getVoidTy(ctx));
}

llvm::AllocaInst * ASTNodeVarByRef::codegen(llvm::Module &, llvm::IRBuilder<> &, llvm::LLVMContext &, CodegenData & cdg) {
    AllocaInst * var = cdg.vars[_var];
    if (!var) {
        //TODO error
    }
    return var;
}