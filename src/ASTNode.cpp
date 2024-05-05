#include "ASTNode.hpp"
#include "LLVMTypeResolver.hpp"
#include <stack>
#include <variant>

static llvm::AllocaInst * CreateEntryBlockAlloca(llvm::Function * f, llvm::Type * type,
                                           const std::string & name) {
    llvm::IRBuilder<> TmpB(&f->getEntryBlock(), f->getEntryBlock().begin());
    llvm::AllocaInst * alloca = TmpB.CreateAlloca(type, nullptr, name);
    TmpB.CreateStore(llvm::Constant::getNullValue(alloca->getAllocatedType()), alloca);
    return alloca;
}

llvm::Value * ASTNodeInt::codegen(llvm::Module &, llvm::IRBuilder<> &,
                                  llvm::LLVMContext & ctx, CodegenData &) {
    return llvm::ConstantInt::get(ctx, llvm::APInt(32, _val, true));
}

llvm::Value *
ASTNodeIdentifier::codegen(llvm::Module &,
                                         llvm::IRBuilder<> & builder,
                                         llvm::LLVMContext &,
                                         CodegenData & cdg) {
    if (auto x = cdg.consts->lookup(_name); x.has_value()) {
        return x.value();
    } else {
        auto val_opt = cdg.vars->lookup(_name);
        assert(val_opt.has_value());
        llvm::AllocaInst * val = val_opt.value();
        return builder.CreateLoad(val->getAllocatedType(), val, _name);
    }
}

llvm::Value * ASTNodeUnary::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx, CodegenData & cdg) {
    llvm::Value * arg = _arg->codegen(module, builder, ctx, cdg);
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
    llvm::Value * lhs = _lhs->codegen(module, builder, ctx, cdg);
    llvm::Value * rhs = _rhs->codegen(module, builder, ctx, cdg);

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
        return builder.CreateSRem(lhs, rhs, "mod_tmp");
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

llvm::Function *
ASTNodePrototype::codegen(llvm::Module & module, llvm::IRBuilder<> &,
                          llvm::LLVMContext & ctx,
                          CodegenData &) {
    std::vector<llvm::Type *> arg_types(_args.size(),
                                        llvm::Type::getInt32Ty(ctx));

    auto t = _return_type->as_variant();
    LLVMTypeResolver llvmtr(ctx);
    llvm::Type * ret_type = std::visit(llvmtr, t);

    llvm::FunctionType * ft = llvm::FunctionType::get(ret_type, arg_types, false);
    llvm::Function * f =
        llvm::Function::Create(ft, llvm::Function::ExternalLinkage, _fn_name, module);

    auto it = _args.begin();
    for (auto & arg : f->args()) {
        arg.setName(it->name);
        ++it;
    }
    return f;
}

llvm::Function *
ASTNodeFunction::codegen(llvm::Module & module, llvm::IRBuilder<> & builder,
                         llvm::LLVMContext & ctx,
                         CodegenData & cdg) {
    llvm::Function * function = module.getFunction(_proto->name());

    if (!function) {
        function = _proto->codegen(module, builder, ctx, cdg);
    }

    if (!function) {
        return nullptr;
    }

    if (!function->empty()) {
        // TODO error
    }

    llvm::BasicBlock * entry = llvm::BasicBlock::Create(ctx, "entry", function);
    llvm::BasicBlock * old_insert_point = builder.GetInsertBlock();
    builder.SetInsertPoint(entry);

    auto old_vars = cdg.vars;
    auto old_consts = cdg.consts;
    cdg.vars = cdg.vars->derive();
    cdg.consts = cdg.consts->derive();
    // insert return value var
    if (function->getReturnType()->getTypeID() != llvm::Type::VoidTyID) {
        llvm::AllocaInst * ret_var =
            CreateEntryBlockAlloca(function, function->getReturnType(),
                                   function->getName().str());
        cdg.vars->data[function->getName().str()] = ret_var;
    }

    for (auto & arg : function->args()) {
        llvm::AllocaInst * alloca = CreateEntryBlockAlloca(
            function, arg.getType(), arg.getName().str());
        builder.CreateStore(&arg, alloca);
        cdg.vars->data[arg.getName().str()] = alloca;
    }

    _block->codegen(module, builder, ctx, cdg);

    // TODO body
    builder.SetInsertPoint(entry);
    llvm::BasicBlock * function_body = llvm::BasicBlock::Create(ctx, "function_body", function);
    builder.CreateBr(function_body);
    builder.SetInsertPoint(function_body);
    _body->codegen(module, builder, ctx, cdg);

    llvm::Value * ret_val = nullptr;
    if (function->getReturnType()->getTypeID() != llvm::Type::VoidTyID) {
        // load return value if expected
        ret_val = builder.CreateLoad(function->getReturnType(),
                                     cdg.vars->data[function->getName().str()],
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
    llvm::Function * function = module.getFunction(_fn);
    if (!function) {
        //TODO error
    }

    std::vector<llvm::Value*> args;
    for (auto & arg : _args) {
        args.push_back(arg->codegen(module, builder, ctx, cdg));
        //TODO compile error handling
    }
    return builder.CreateCall(function, args, "call_tmp");
}

llvm::Value * ASTNodeIf::codegen(llvm::Module & module,
                                 llvm::IRBuilder<> & builder,
                                 llvm::LLVMContext & ctx, CodegenData & cdg) {
    llvm::Value * cond = _cond->codegen(module, builder, ctx, cdg);
    llvm::Value * null = llvm::Constant::getNullValue(cond->getType());
    cond = builder.CreateICmpNE(cond, null, "if_cond");

    llvm::Function * function = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock * then_bb = llvm::BasicBlock::Create(ctx, "then_block", function);
    llvm::BasicBlock * else_bb = llvm::BasicBlock::Create(ctx, "else_block");
    llvm::BasicBlock * cont = llvm::BasicBlock::Create(ctx, "after_block");
    builder.CreateCondBr(cond, then_bb, else_bb);

    //then branch
    builder.SetInsertPoint(then_bb);
    // TODO body
    _body->codegen(module, builder, ctx, cdg);
    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(cont);
    }

    //else branch
    function->insert(function->end(), else_bb);
    builder.SetInsertPoint(else_bb);
    //TODO body
    if (_else.has_value()) {
        _else.value()->codegen(module, builder, ctx, cdg);
    }
    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(cont);
    }

    //after if block
    function->insert(function->end(), cont);
    builder.SetInsertPoint(cont);
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeBody::codegen(llvm::Module & module,
                                   llvm::IRBuilder<> & builder,
                                   llvm::LLVMContext & ctx, CodegenData & cdg) {
    auto old_vars = cdg.vars;
    auto old_consts = cdg.consts;
    cdg.vars = cdg.vars->derive();
    cdg.consts = cdg.consts->derive();
    for (auto & stmt : _stmts) {
        stmt->codegen(module, builder, ctx, cdg);
        if (builder.GetInsertBlock()->getTerminator()) {
            break;
        }
    }
    cdg.vars = old_vars;
    cdg.consts = old_consts;
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeExit::codegen(llvm::Module &, llvm::IRBuilder<> & builder,
                                   llvm::LLVMContext &, CodegenData & cdg) {
    llvm::Function * function = builder.GetInsertBlock()->getParent();
    llvm::Value * ret = nullptr;
    if (function->getReturnType()->getTypeID() != llvm::Type::VoidTyID) {
        llvm::AllocaInst * ret_alloca = cdg.vars->lookup(function->getName().str()).value();
        ret = builder.CreateLoad(function->getReturnType(), ret_alloca, "return_value");
    }
    return builder.CreateRet(ret);
}

llvm::Value * ASTNodeFor::codegen(llvm::Module & module,
                                  llvm::IRBuilder<> & builder,
                                  llvm::LLVMContext & ctx, CodegenData & cdg) {
    auto old_vars = cdg.vars;
    auto old_consts = cdg.consts;
    cdg.vars = cdg.vars->derive();
    cdg.consts = cdg.consts->derive();
    llvm::Value * init_val = _it_start->codegen(module, builder, ctx, cdg);
    //TODO compile error handling

    llvm::Function * function = builder.GetInsertBlock()->getParent();
    llvm::AllocaInst * loop_var = nullptr;
    if (!cdg.vars->lookup(_var).has_value()) {
        loop_var =
            CreateEntryBlockAlloca(function, llvm::Type::getInt32Ty(ctx), _var);
        cdg.vars->data[_var] = loop_var;
    } else {
        loop_var = cdg.vars->lookup(_var).value();
    }
    builder.CreateStore(init_val, loop_var, "iter_var_init");

    // main loop body
    llvm::BasicBlock * loop_cond = llvm::BasicBlock::Create(ctx, "loop_cond", function);
    llvm::BasicBlock * loop_bb = llvm::BasicBlock::Create(ctx, "loop", function);
    llvm::BasicBlock * loop_end = llvm::BasicBlock::Create(ctx, "loop_end");
    cdg.break_addrs.push(loop_end);
    cdg.cont_addrs.push(loop_cond);
    builder.CreateBr(loop_cond);
    builder.SetInsertPoint(loop_cond);
    llvm::Value * current_val = builder.CreateLoad(llvm::Type::getInt32Ty(ctx), loop_var, "loop_current_val");
    llvm::Value * end_cond = _it_stop->codegen(module, builder, ctx, cdg);
    llvm::Value * loop_cont_cond = builder.CreateICmpNE(current_val, end_cond, "loop_cont_cond");
    builder.CreateCondBr(loop_cont_cond, loop_bb, loop_end);

    builder.SetInsertPoint(loop_bb);
    _body->codegen(module, builder, ctx, cdg);
    llvm::Value * step = llvm::ConstantInt::get(ctx, llvm::APInt(32, 1, true));

    if (!builder.GetInsertBlock()->getTerminator()) {
        // mutate iter var
        current_val = builder.CreateLoad(llvm::Type::getInt32Ty(ctx), loop_var, "loop_var_fetch");
        llvm::Value * next_val = nullptr;
        if (_is_downto) {
            next_val = builder.CreateSub(current_val, step, "step");
        } else {
            next_val = builder.CreateAdd(current_val, step, "step");
        }
        builder.CreateStore(next_val, loop_var);
        builder.CreateBr(loop_cond);
    }
    function->insert(function->end(), loop_end);
    builder.SetInsertPoint(loop_end);

    cdg.break_addrs.pop();
    cdg.cont_addrs.pop();
    cdg.vars = old_vars;
    cdg.consts = old_consts;
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeAssign::codegen(llvm::Module & module,
                                     llvm::IRBuilder<> & builder,
                                     llvm::LLVMContext & ctx,
                                     CodegenData & cdg) {
    llvm::AllocaInst * var = cdg.vars->lookup(_target).value();
    llvm::Value * value = _rhs->codegen(module, builder, ctx, cdg);
    builder.CreateStore(value, var);
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeBreak::codegen(llvm::Module &, llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx,
                                    CodegenData & cdg) {
    assert(!cdg.break_addrs.empty());
    llvm::BasicBlock * loop_end = cdg.break_addrs.top();
    builder.CreateBr(loop_end);
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeVar::codegen(llvm::Module &, llvm::IRBuilder<> & builder,
                                  llvm::LLVMContext & ctx, CodegenData & cdg) {
    llvm::Function * function = builder.GetInsertBlock()->getParent();
    for (const auto & var : _vars) {
        auto t = var.type->as_variant();
        LLVMTypeResolver llvmtr(ctx);
        llvm::Type * type = std::visit(llvmtr, t);
        llvm::AllocaInst * alloca = CreateEntryBlockAlloca(function, type, var.name);
        cdg.vars->data[var.name] = alloca;
    }
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeConst::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx, CodegenData & cdg) {
    llvm::Function * function = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock * current_block = builder.GetInsertBlock();
    llvm::BasicBlock * entry = &function->getEntryBlock();
    builder.SetInsertPoint(entry);
    for (const auto & c : _constants) {
        llvm::Value * val = c.value->codegen(module, builder, ctx, cdg);
        cdg.consts->data[c.name] = val;
    }
    builder.SetInsertPoint(current_block);
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeBlock::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx, CodegenData & cdg) {
    for (auto & block : _decls) {
        block->codegen(module, builder, ctx, cdg);
    }
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeWhile::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx, CodegenData & cdg) {
    auto old_vars = cdg.vars;
    auto old_consts = cdg.consts;
    cdg.vars = cdg.vars->derive();
    cdg.consts = cdg.consts->derive();

    llvm::Function * function = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock * loop_cond = llvm::BasicBlock::Create(ctx, "while_cond", function);
    llvm::BasicBlock * loop_body = llvm::BasicBlock::Create(ctx, "while_body", function);
    llvm::BasicBlock * loop_end = llvm::BasicBlock::Create(ctx, "while_end");
    cdg.break_addrs.push(loop_end);

    builder.CreateBr(loop_cond);
    builder.SetInsertPoint(loop_cond);
    llvm::Value * cond = _cond->codegen(module, builder, ctx, cdg);
    llvm::Value * null = llvm::Constant::getNullValue(cond->getType());
    llvm::Value * end_cond = builder.CreateICmpNE(cond, null, "end_cond");
    builder.CreateCondBr(end_cond, loop_body, loop_end);

    builder.SetInsertPoint(loop_body);
    _body->codegen(module, builder, ctx, cdg);
    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(loop_cond);
    }

    function->insert(function->end(), loop_end);
    builder.SetInsertPoint(loop_end);
    cdg.vars = old_vars;
    cdg.consts = old_consts;
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::AllocaInst * ASTNodeVarByRef::codegen(llvm::Module &, llvm::IRBuilder<> &, llvm::LLVMContext &, CodegenData & cdg) {
    llvm::AllocaInst * var = cdg.vars->lookup(_var).value();
    return var;
}