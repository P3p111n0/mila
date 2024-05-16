#include "ASTNode.hpp"
#include "LLVMTypeResolver.hpp"
#include <TypeInfo.hpp>
#include <stack>
#include <variant>

static llvm::AllocaInst * CreateEntryBlockAlloca(llvm::Function * f,
                                                 llvm::Type * type,
                                                 const std::string & name) {
    llvm::IRBuilder<> TmpB(&f->getEntryBlock(), f->getEntryBlock().begin());
    llvm::AllocaInst * alloca = TmpB.CreateAlloca(type, nullptr, name);
    TmpB.CreateStore(llvm::Constant::getNullValue(alloca->getAllocatedType()),
                     alloca);
    return alloca;
}

static llvm::Type * resolve_llvm_type(llvm::LLVMContext & ctx,
                                      std::shared_ptr<Type> t) {
    LLVMTypeResolver llvmtr(ctx);
    return std::visit(llvmtr, t->as_variant());
}

static llvm::Type * get_ptr_elem_type(llvm::LLVMContext & ctx,
                                      ASTNodeFunction * f, int n) {
    auto arg_it = f->proto->args.begin();
    std::advance(arg_it, n);
    type_ptr arg_type = (*arg_it).type;
    assert(type_info::is_ref_type(arg_type));
    std::shared_ptr<RefType> ref = type_info::to_ref_type(arg_type);
    assert(type_info::is_base_type(ref->base));
    return resolve_llvm_type(ctx, ref->base);
}

llvm::Value * ASTNodeInt::codegen(llvm::Module &, llvm::IRBuilder<> &,
                                  llvm::LLVMContext & ctx, CodegenData &) {
    return llvm::ConstantInt::get(ctx, llvm::APInt(32, val, true));
}

llvm::Value * ASTNodeDouble::codegen(llvm::Module &, llvm::IRBuilder<> &,
                                     llvm::LLVMContext & ctx, CodegenData &) {
    return llvm::ConstantFP::get(ctx, llvm::APFloat(val));
}

llvm::Value * ASTNodeIdentifier::codegen(llvm::Module &,
                                         llvm::IRBuilder<> & builder,
                                         llvm::LLVMContext &,
                                         CodegenData & cdg) {
    if (auto x =
            cdg.consts->lookup(name, ValMap<llvm::Value *>::Scope::Function)) {
        return cdg.consts->lookup(name).value();
    } else {
        auto val_opt =
            cdg.vars->lookup(name, ValMap<MemoryLocation>::Scope::Function);
        assert(val_opt.has_value());
        MemoryLocation val = val_opt.value();
        return builder.CreateLoad(val.pointee_type, val.ptr, name);
    }

    assert(0 && "unreachable");
}

llvm::Value * ASTNodeIdentifier::get_allocated_ptr(llvm::Module &,
                                                   llvm::IRBuilder<> &,
                                                   llvm::LLVMContext &,
                                                   CodegenData & cdg) const {
    auto var_lookup = cdg.vars->lookup(name);
    assert(var_lookup.has_value());
    return var_lookup.value().ptr;
}

llvm::Value * ASTNodeUnary::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx,
                                    CodegenData & cdg) {
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

llvm::Value * ASTNodeFBinary::codegen(llvm::Module & module,
                                      llvm::IRBuilder<> & builder,
                                      llvm::LLVMContext & ctx,
                                      CodegenData & cdg) {
    llvm::Value * lhs_val = lhs->codegen(module, builder, ctx, cdg);
    llvm::Value * rhs_val = rhs->codegen(module, builder, ctx, cdg);

    switch (op) {
    case Operator::Add:
        return builder.CreateFAdd(lhs_val, rhs_val, "fadd_tmp");
    case Operator::Sub:
        return builder.CreateFSub(lhs_val, rhs_val, "fsub_tmp");
    case Operator::Mul:
        return builder.CreateFMul(lhs_val, rhs_val, "fmul_tmp");
    case Operator::Div:
        return builder.CreateFDiv(lhs_val, rhs_val, "fdiv_tmp");
    case Operator::Mod:
        return builder.CreateFRem(lhs_val, rhs_val, "fmod_tmp");
    case Operator::Eq:
        return builder.CreateFCmpOEQ(lhs_val, rhs_val, "foeq_tmp");
    case Operator::NEq:
        return builder.CreateFCmpONE(lhs_val, rhs_val, "foneq_tmp");
    case Operator::Lt:
        return builder.CreateFCmpOLT(lhs_val, rhs_val, "folt_tmp");
    case Operator::Gt:
        return builder.CreateFCmpOGT(lhs_val, rhs_val, "fogt_tmp");
    case Operator::LtE:
        return builder.CreateFCmpOLE(lhs_val, rhs_val, "folte_tmp");
    case Operator::GtE:
        return builder.CreateFCmpOGE(lhs_val, rhs_val, "fogte_tmp");
    default:
        return nullptr;
    }
}

llvm::Function * ASTNodePrototype::codegen(llvm::Module & module,
                                           llvm::IRBuilder<> &,
                                           llvm::LLVMContext & ctx,
                                           CodegenData &) {
    std::vector<llvm::Type *> arg_types;
    for (auto & arg : args) {
        arg_types.emplace_back(resolve_llvm_type(ctx, arg.type));
    }

    llvm::Type * ret_type = resolve_llvm_type(ctx, return_type);

    llvm::FunctionType * ft =
        llvm::FunctionType::get(ret_type, arg_types, false);
    llvm::Function * f = llvm::Function::Create(
        ft, llvm::Function::ExternalLinkage, fn_name, module);

    auto it = args.begin();
    for (auto & arg : f->args()) {
        arg.setName(it->name);
        ++it;
    }
    return f;
}

llvm::Function * ASTNodeFunction::codegen(llvm::Module & module,
                                          llvm::IRBuilder<> & builder,
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
        assert(0 && "function conflict");
    }

    llvm::BasicBlock * entry = llvm::BasicBlock::Create(ctx, "entry", function);
    llvm::BasicBlock * old_insert_point = builder.GetInsertBlock();
    builder.SetInsertPoint(entry);

    auto old_vars = cdg.vars;
    auto old_consts = cdg.consts;
    cdg.vars = cdg.vars->derive(ValMap<MemoryLocation>::Scope::Function);
    cdg.consts = cdg.consts->derive(ValMap<llvm::Value *>::Scope::Function);
    // insert return value var
    if (function->getReturnType()->getTypeID() != llvm::Type::VoidTyID) {
        llvm::AllocaInst * ret_var = CreateEntryBlockAlloca(
            function, function->getReturnType(), function->getName().str());
        cdg.vars->data[function->getName().str()] = {ret_var,
                                                     function->getReturnType()};
    }

    for (auto & arg : function->args()) {
        if (arg.getType()->isPointerTy()) {
            llvm::Type * pointee = get_ptr_elem_type(ctx, this, arg.getArgNo());
            cdg.vars->data[arg.getName().str()] = {&arg, pointee};
        } else {
            llvm::AllocaInst * alloca = CreateEntryBlockAlloca(
                function, arg.getType(), arg.getName().str());
            builder.CreateStore(&arg, alloca);
            cdg.vars->data[arg.getName().str()] = {alloca,
                                                   alloca->getAllocatedType()};
        }
    }

    block->codegen(module, builder, ctx, cdg);

    // TODO body
    builder.SetInsertPoint(entry);
    llvm::BasicBlock * function_body =
        llvm::BasicBlock::Create(ctx, "function_body", function);
    builder.CreateBr(function_body);
    builder.SetInsertPoint(function_body);
    body->codegen(module, builder, ctx, cdg);

    llvm::Value * ret_val = nullptr;
    if (function->getReturnType()->getTypeID() != llvm::Type::VoidTyID) {
        // load return value if expected
        ret_val = builder.CreateLoad(
            function->getReturnType(),
            cdg.vars->data[function->getName().str()].ptr, "return_value");
    }

    builder.CreateRet(ret_val);
    assert(!verifyFunction(*function, &llvm::errs()));
    //   restore symbol table
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
        // TODO error
    }

    std::vector<llvm::Value *> args_;
    for (auto & arg : args) {
        args_.push_back(arg->codegen(module, builder, ctx, cdg));
        // TODO compile error handling
    }

    if (function->getReturnType()->isVoidTy()) {
        return builder.CreateCall(function, args_);
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
    llvm::BasicBlock * then_bb =
        llvm::BasicBlock::Create(ctx, "then_block", function);
    llvm::BasicBlock * else_bb = llvm::BasicBlock::Create(ctx, "else_block");
    llvm::BasicBlock * cont = llvm::BasicBlock::Create(ctx, "after_block");
    builder.CreateCondBr(cond_val, then_bb, else_bb);

    // then branch
    builder.SetInsertPoint(then_bb);
    // TODO body
    body->codegen(module, builder, ctx, cdg);
    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(cont);
    }

    // else branch
    function->insert(function->end(), else_bb);
    builder.SetInsertPoint(else_bb);
    // TODO body
    if (else_.has_value()) {
        else_.value()->codegen(module, builder, ctx, cdg);
    }
    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(cont);
    }

    // after if block
    function->insert(function->end(), cont);
    builder.SetInsertPoint(cont);
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeBody::codegen(llvm::Module & module,
                                   llvm::IRBuilder<> & builder,
                                   llvm::LLVMContext & ctx, CodegenData & cdg) {
    auto old_vars = cdg.vars;
    auto old_consts = cdg.consts;
    cdg.vars = cdg.vars->derive(ValMap<MemoryLocation>::Scope::Body);
    cdg.consts = cdg.consts->derive(ValMap<llvm::Value*>::Scope::Body);
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
        llvm::Value * ret_alloca =
            cdg.vars->lookup(function->getName().str()).value().ptr;
        ret = builder.CreateLoad(function->getReturnType(), ret_alloca,
                                 "return_value");
    }
    return builder.CreateRet(ret);
}

llvm::Value * ASTNodeFor::codegen(llvm::Module & module,
                                  llvm::IRBuilder<> & builder,
                                  llvm::LLVMContext & ctx, CodegenData & cdg) {
    auto old_vars = cdg.vars;
    auto old_consts = cdg.consts;
    cdg.vars = cdg.vars->derive(ValMap<MemoryLocation>::Scope::Loop);
    cdg.consts = cdg.consts->derive(ValMap<llvm::Value *>::Scope::Loop);
    llvm::Value * init_val = it_start->codegen(module, builder, ctx, cdg);
    // TODO compile error handling

    llvm::Function * function = builder.GetInsertBlock()->getParent();
    llvm::Value * loop_var = nullptr;
    if (!cdg.vars->lookup(var).has_value()) {
        loop_var =
            CreateEntryBlockAlloca(function, llvm::Type::getInt32Ty(ctx), var);
        cdg.vars->data[var] = {loop_var, llvm::Type::getInt32Ty(ctx)};
    } else {
        loop_var = cdg.vars->lookup(var).value().ptr;
    }
    builder.CreateStore(init_val, loop_var);

    // main loop body
    llvm::BasicBlock * loop_cond =
        llvm::BasicBlock::Create(ctx, "loop_cond", function);
    llvm::BasicBlock * loop_bb =
        llvm::BasicBlock::Create(ctx, "loop", function);
    llvm::BasicBlock * loop_end = llvm::BasicBlock::Create(ctx, "loop_end");
    llvm::BasicBlock * var_mutate = llvm::BasicBlock::Create(ctx, "var_mutate");

    cdg.break_addrs.push(loop_end);
    cdg.cont_addrs.push(var_mutate);
    builder.CreateBr(loop_cond);
    builder.SetInsertPoint(loop_cond);
    llvm::Value * current_val = builder.CreateLoad(
        llvm::Type::getInt32Ty(ctx), loop_var, "loop_current_val");
    llvm::Value * end_cond = it_stop->codegen(module, builder, ctx, cdg);
    llvm::Value * loop_cont_cond;
    if (is_downto) {
        loop_cont_cond =
            builder.CreateICmpSGE(current_val, end_cond, "loop_cont_cond");
    } else {
        loop_cont_cond =
            builder.CreateICmpSLE(current_val, end_cond, "loop_cont_cond");
    }
    builder.CreateCondBr(loop_cont_cond, loop_bb, loop_end);

    builder.SetInsertPoint(loop_bb);
    body->codegen(module, builder, ctx, cdg);
    llvm::Value * step = llvm::ConstantInt::get(ctx, llvm::APInt(32, 1, true));

    if (!builder.GetInsertBlock()->getTerminator()) {
        builder.CreateBr(var_mutate);
    }

    // mutate iter var
    function->insert(function->end(), var_mutate);
    builder.SetInsertPoint(var_mutate);
    current_val = builder.CreateLoad(llvm::Type::getInt32Ty(ctx), loop_var,
                                     "loop_var_fetch");
    llvm::Value * next_val = nullptr;
    if (is_downto) {
        next_val = builder.CreateSub(current_val, step, "step");
    } else {
        next_val = builder.CreateAdd(current_val, step, "step");
    }
    builder.CreateStore(next_val, loop_var);
    builder.CreateBr(loop_cond);

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
    llvm::Value * var = target->get_allocated_ptr(module, builder, ctx, cdg);
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

llvm::Value * ASTNodeContinue::codegen(llvm::Module &,
                                       llvm::IRBuilder<> & builder,
                                       llvm::LLVMContext & ctx,
                                       CodegenData & cdg) {
    assert(!cdg.cont_addrs.empty());
    llvm::BasicBlock * mutate_block = cdg.cont_addrs.top();
    builder.CreateBr(mutate_block);
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeVar::codegen(llvm::Module &, llvm::IRBuilder<> & builder,
                                  llvm::LLVMContext & ctx, CodegenData & cdg) {
    llvm::Function * function = builder.GetInsertBlock()->getParent();
    for (const auto & var : vars) {
        llvm::Type * type = resolve_llvm_type(ctx, var.type);
        llvm::AllocaInst * alloca =
            CreateEntryBlockAlloca(function, type, var.name);
        cdg.vars->data[var.name] = {alloca, alloca->getAllocatedType()};
    }
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeConst::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx,
                                    CodegenData & cdg) {
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
                                    llvm::LLVMContext & ctx,
                                    CodegenData & cdg) {
    for (auto & block : decls) {
        block->codegen(module, builder, ctx, cdg);
    }
    return llvm::Constant::getNullValue(llvm::Type::getVoidTy(ctx));
}

llvm::Value * ASTNodeWhile::codegen(llvm::Module & module,
                                    llvm::IRBuilder<> & builder,
                                    llvm::LLVMContext & ctx,
                                    CodegenData & cdg) {
    auto old_vars = cdg.vars;
    auto old_consts = cdg.consts;
    cdg.vars = cdg.vars->derive(ValMap<MemoryLocation>::Scope::Loop);
    cdg.consts = cdg.consts->derive(ValMap<llvm::Value *>::Scope::Loop);

    llvm::Function * function = builder.GetInsertBlock()->getParent();
    llvm::BasicBlock * loop_cond =
        llvm::BasicBlock::Create(ctx, "while_cond", function);
    llvm::BasicBlock * loop_body =
        llvm::BasicBlock::Create(ctx, "while_body", function);
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

llvm::Value * ASTNodeVarByRef::codegen(llvm::Module & module,
                                       llvm::IRBuilder<> & builder,
                                       llvm::LLVMContext & ctx,
                                       CodegenData & cdg) {
    return var->get_allocated_ptr(module, builder, ctx, cdg);
}

llvm::Value * ASTNodeTypeCast::codegen(llvm::Module & module,
                                       llvm::IRBuilder<> & builder,
                                       llvm::LLVMContext & ctx,
                                       CodegenData & cdg) {
    llvm::Type * dst_t = resolve_llvm_type(ctx, dst);

    auto arg_val = arg->codegen(module, builder, ctx, cdg);

    if (dst_t->isDoubleTy()) {
        return builder.CreateSIToFP(arg_val, dst_t);
    } else if (dst_t->isIntegerTy()) {
        return builder.CreateFPToSI(arg_val, dst_t);
    } else {
        assert(0 && "this shouldn't happen");
    }
}

llvm::Value * ASTNodeBuiltinCall::codegen(llvm::Module &, llvm::IRBuilder<> &,
                                          llvm::LLVMContext &, CodegenData &) {
    assert(0 && "node intended only as an intermediate");
}

llvm::Value * ASTNodeString::codegen(llvm::Module & module, llvm::IRBuilder<> &,
                                     llvm::LLVMContext & ctx, CodegenData &) {
    auto char_type = llvm::IntegerType::get(ctx, 8);
    auto array_type = llvm::ArrayType::get(
        char_type, str.length() + 1); // + 1 for the terminating zero byte

    llvm::Constant * initializer = llvm::ConstantDataArray::getString(ctx, str);
    llvm::GlobalVariable * global_str = new llvm::GlobalVariable(
        module, array_type, true,
        llvm::GlobalVariable::LinkageTypes::PrivateLinkage, initializer,
        ".str");
    return llvm::ConstantExpr::getBitCast(global_str,
                                          char_type->getPointerTo());
}

llvm::Value * ASTNodeArrAccess::codegen(llvm::Module & module,
                                        llvm::IRBuilder<> & builder,
                                        llvm::LLVMContext & ctx,
                                        CodegenData & cdg) {
    llvm::Value * ptr = get_allocated_ptr(module, builder, ctx, cdg);
    auto alloca_lookup = cdg.vars->lookup(name);
    assert(alloca_lookup.has_value());
    llvm::Type * type = alloca_lookup.value().pointee_type;
    for (size_t i = 0; i < idx_list.size(); i++) {
        assert(type->isArrayTy());
        type = type->getArrayElementType();
    }

    return builder.CreateLoad(type, ptr);
}

llvm::Value * ASTNodeArrAccess::get_allocated_ptr(llvm::Module & module,
                                                  llvm::IRBuilder<> & builder,
                                                  llvm::LLVMContext & ctx,
                                                  CodegenData & cdg) const {
    std::vector<llvm::Value *> idxs;
    // not sure why this is here, but it works
    // https://stackoverflow.com/questions/64838994/correct-use-of-llvm-irbuildercreategep
    idxs.emplace_back(llvm::ConstantInt::get(ctx, llvm::APInt(32, 0, true)));
    for (auto & index : idx_list) {
        idxs.emplace_back(index->codegen(module, builder, ctx, cdg));
    }

    auto alloca_lookup = cdg.vars->lookup(name);
    assert(alloca_lookup.has_value());
    MemoryLocation mem = alloca_lookup.value();

    return builder.CreateGEP(mem.pointee_type, mem.ptr, idxs, name + "_gep",
                             true);
}