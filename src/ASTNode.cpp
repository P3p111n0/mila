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
    return llvm::ConstantInt::get(ctx, llvm::APInt(32, val, true));
}

llvm::Value *
ASTNodeIdentifier::codegen(llvm::Module &,
                                         llvm::IRBuilder<> & builder,
                                         llvm::LLVMContext &,
                                         CodegenData & cdg) {
    if (auto x = cdg.consts->lookup(name); x.has_value()) {
        return x.value();
    } else {
        auto val_opt = cdg.vars->lookup(name);
        assert(val_opt.has_value());
        llvm::AllocaInst * val = val_opt.value();
        return builder.CreateLoad(val->getAllocatedType(), val, name);
    }
}

llvm::Value * ASTNodeUnary::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx, CodegenData & cdg) {
    llvm::Value * arg_val = arg->codegen(module, builder, ctx, cdg);
    switch (op) {
    case Operator::Not:
        return builder.CreateNot(arg_val, "not_tmp");
    default:
        return nullptr;
    }
}

llvm::Value * ASTNodeBinary::codegen(llvm::Module & module,
                                     llvm::IRBuilder<> & builder,
                                     llvm::LLVMContext & ctx,
                                     CodegenData & cdg) {
    llvm::Value * lhs_val = lhs->codegen(module, builder, ctx, cdg);
    llvm::Value * rhs_val = rhs->codegen(module, builder, ctx, cdg);

    switch (op) {
    case Operator::Add:
        return builder.CreateAdd(lhs_val, rhs_val, "add_tmp");
    case Operator::Sub:
        return builder.CreateSub(lhs_val, rhs_val, "sub_tmp");
    case Operator::Mul:
        return builder.CreateMul(lhs_val, rhs_val, "mul_tmp");
    case Operator::Div:
        return builder.CreateSDiv(lhs_val, rhs_val, "div_tmp");
    case Operator::Or:
        return builder.CreateOr(lhs_val, rhs_val, "or_tmp");
    case Operator::Xor:
        return builder.CreateXor(lhs_val, rhs_val, "xor_tmp");
    case Operator::And:
        return builder.CreateAnd(lhs_val, rhs_val, "and_tmp");
    case Operator::Mod:
        return builder.CreateSRem(lhs_val, rhs_val, "mod_tmp");
    case Operator::Eq:
        return builder.CreateICmpEQ(lhs_val, rhs_val, "eq_tmp");
    case Operator::NEq:
        return builder.CreateICmpNE(lhs_val, rhs_val, "neq_tmp");
    case Operator::Lt:
        return builder.CreateICmpSLT(lhs_val, rhs_val, "lt_tmp");
    case Operator::Gt:
        return builder.CreateICmpSGT(lhs_val, rhs_val, "gt_tmp");
    case Operator::LtE:
        return builder.CreateICmpSLE(lhs_val, rhs_val, "lte_tmp");
    case Operator::GtE:
        return builder.CreateICmpSGE(lhs_val, rhs_val, "gte_tmp");
    default:
        return nullptr;
    }
}

llvm::Function *
ASTNodePrototype::codegen(llvm::Module & module, llvm::IRBuilder<> &,
                          llvm::LLVMContext & ctx,
                          CodegenData &) {
    std::vector<llvm::Type *> arg_types(args.size(),
                                        llvm::Type::getInt32Ty(ctx));

    auto t = return_type->as_variant();
    LLVMTypeResolver llvmtr(ctx);
    llvm::Type * ret_type = std::visit(llvmtr, t);

    llvm::FunctionType * ft = llvm::FunctionType::get(ret_type, arg_types, false);
    llvm::Function * f =
        llvm::Function::Create(ft, llvm::Function::ExternalLinkage, fn_name, module);

    auto it = args.begin();
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
    llvm::Function * function = module.getFunction(proto->name());

    if (!function) {
        function = proto->codegen(module, builder, ctx, cdg);
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

    block->codegen(module, builder, ctx, cdg);

    // TODO body
    builder.SetInsertPoint(entry);
    llvm::BasicBlock * function_body = llvm::BasicBlock::Create(ctx, "function_body", function);
    builder.CreateBr(function_body);
    builder.SetInsertPoint(function_body);
    body->codegen(module, builder, ctx, cdg);

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
    llvm::Function * function = module.getFunction(fn);
    if (!function) {
        //TODO error
    }

    std::vector<llvm::Value*> args_;
    for (auto & arg : args) {
        args_.push_back(arg->codegen(module, builder, ctx, cdg));
        //TODO compile error handling
    }
    return builder.CreateCall(function, args_, "call_tmp");
}

llvm::Value * ASTNodeIf::codegen(llvm::Module & module,
                                 llvm::IRBuilder<> & builder,
                                 llvm::LLVMContext & ctx, CodegenData & cdg) {
    llvm::Value * cond_val = cond->codegen(module, builder, ctx, cdg);
    llvm::Value * null = llvm::Constant::getNullValue(cond_val->getType());
    cond_val = builder.CreateICmpNE(cond_val, null, "if_cond");

    llvm::Function * function = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock * then_bb = llvm::BasicBlock::Create(ctx, "then_block", function);
    llvm::BasicBlock * else_bb = llvm::BasicBlock::Create(ctx, "else_block");
    llvm::BasicBlock * cont = llvm::BasicBlock::Create(ctx, "after_block");
    builder.CreateCondBr(cond_val, then_bb, else_bb);

    //then branch
    builder.SetInsertPoint(then_bb);
    // TODO body
    body->codegen(module, builder, ctx, cdg);
    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(cont);
    }

    //else branch
    function->insert(function->end(), else_bb);
    builder.SetInsertPoint(else_bb);
    //TODO body
    if (else_.has_value()) {
        else_.value()->codegen(module, builder, ctx, cdg);
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
    for (auto & stmt : stmts) {
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
    llvm::Value * init_val = it_start->codegen(module, builder, ctx, cdg);
    //TODO compile error handling

    llvm::Function * function = builder.GetInsertBlock()->getParent();
    llvm::AllocaInst * loop_var = nullptr;
    if (!cdg.vars->lookup(var).has_value()) {
        loop_var =
            CreateEntryBlockAlloca(function, llvm::Type::getInt32Ty(ctx), var);
        cdg.vars->data[var] = loop_var;
    } else {
        loop_var = cdg.vars->lookup(var).value();
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
    llvm::Value * end_cond = it_stop->codegen(module, builder, ctx, cdg);
    llvm::Value * loop_cont_cond = builder.CreateICmpNE(current_val, end_cond, "loop_cont_cond");
    builder.CreateCondBr(loop_cont_cond, loop_bb, loop_end);

    builder.SetInsertPoint(loop_bb);
    body->codegen(module, builder, ctx, cdg);
    llvm::Value * step = llvm::ConstantInt::get(ctx, llvm::APInt(32, 1, true));

    if (!builder.GetInsertBlock()->getTerminator()) {
        // mutate iter var
        current_val = builder.CreateLoad(llvm::Type::getInt32Ty(ctx), loop_var, "loop_var_fetch");
        llvm::Value * next_val = nullptr;
        if (is_downto) {
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
    llvm::AllocaInst * var = cdg.vars->lookup(target).value();
    llvm::Value * value = rhs->codegen(module, builder, ctx, cdg);
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
    for (const auto & var : vars) {
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
    for (const auto & c : constants) {
        llvm::Value * val = c.value->codegen(module, builder, ctx, cdg);
        cdg.consts->data[c.name] = val;
    }
    builder.SetInsertPoint(current_block);
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeBlock::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx, CodegenData & cdg) {
    for (auto & block : decls) {
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
    llvm::Value * cond_val = cond->codegen(module, builder, ctx, cdg);
    llvm::Value * null = llvm::Constant::getNullValue(cond_val->getType());
    llvm::Value * end_cond = builder.CreateICmpNE(cond_val, null, "end_cond");
    builder.CreateCondBr(end_cond, loop_body, loop_end);

    builder.SetInsertPoint(loop_body);
    body->codegen(module, builder, ctx, cdg);
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
    llvm::AllocaInst * var_ = cdg.vars->lookup(var).value();
    return var_;
}

llvm::Value * ASTNodeTypeCast::codegen(llvm::Module &, llvm::IRBuilder<> &, llvm::LLVMContext &, CodegenData &) {
    assert(0); // TODO
}