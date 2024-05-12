#include "TypeChecker.hpp"
#include "TypeInfo.hpp"

void TypeChecker::errs(std::ostream & os) const {
    for (const auto & err : _errs) {
        os << err << std::endl;
    }
}

std::shared_ptr<ASTNode> TypeChecker::tree_rebuild(ASTNode * main_node) {
    TypeResult main_node_tr = std::visit(*this, main_node->as_variant());
    auto result = std::shared_ptr<ASTNode>(main_node_tr.node);
    if (_errs.empty()) {
        return result;
    }
    return nullptr;
}

TypeResult TypeChecker::operator()(ASTNodeInt * node) {
    return {node->shallow_copy(), type_ptr(_tf.get_int_t())};
}

TypeResult TypeChecker::operator()(ASTNodeString * str) {
    return {str->shallow_copy(), type_ptr(_tf.get_string_t())};
}

TypeResult TypeChecker::operator()(ASTNodeDouble * dval) {
    return {dval->shallow_copy(), type_ptr(_tf.get_double_t())};
}

TypeResult TypeChecker::operator()(ASTNodeUnary * node) {
    TypeResult arg = std::visit(*this, node->arg->as_variant());
    if (!type_info::is_int(arg.type)) {
        _errs.emplace_back("argument of unary operation has to be of type int");
    }
    ASTNodeUnary * new_node = node->shallow_copy();
    new_node->arg = std::shared_ptr<ASTNode>(arg.node);
    return {new_node, arg.type};
}

TypeResult TypeChecker::operator()(ASTNodeBinary * binary_node) {
    TypeResult lhs_res = std::visit(*this, binary_node->lhs->as_variant());
    TypeResult rhs_res = std::visit(*this, binary_node->rhs->as_variant());

    type_ptr common_type =
        type_info::get_common_type(lhs_res.type, rhs_res.type);
    if (!common_type) {
        std::string lhs_id = type_info::get_type_identifier(lhs_res.type);
        std::string rhs_id = type_info::get_type_identifier(rhs_res.type);
        _errs.emplace_back("Cannot convert " + lhs_id + " and " + rhs_id +
                           " to a common type.");
    }

    ASTNode * final_lhs;
    if (!type_info::equal(common_type, lhs_res.type)) {
        final_lhs = new ASTNodeTypeCast(common_type, lhs_res.node);
    } else {
        final_lhs = lhs_res.node;
    }

    ASTNode * final_rhs;
    if (!type_info::equal(common_type, rhs_res.type)) {
        final_rhs = new ASTNodeTypeCast(common_type, rhs_res.node);
    } else {
        final_rhs = rhs_res.node;
    }

    ASTNode * final_binary_node;
    if (type_info::is_int(common_type)) {
        final_binary_node =
            new ASTNodeBinary(final_lhs, final_rhs, binary_node->op);
    } else if (type_info::is_double(common_type)) {
        auto op = type_info::int_to_fp_arithmetic(binary_node->op);
        if (!op.has_value()) {
            _errs.emplace_back("operation not supported for fp type");
            final_binary_node = new ASTNodeFBinary(final_lhs, final_rhs,
                                      ASTNodeFBinary::Operator(-1));
        }
        final_binary_node = new ASTNodeFBinary(final_lhs, final_rhs,
                                               op.value());
    } else {
        assert(0 && "this shouldn't happen");
    }

    return {final_binary_node, type_ptr(common_type)};
}

TypeResult TypeChecker::operator()(ASTNodeFBinary *) {
    assert(0 && "unreachable");
}

TypeResult TypeChecker::operator()(ASTNodeIdentifier * id) {
    auto var_lookup = _st->lookup_variable(id->name);
    auto consts_lookup = _st->lookup_constant(id->name);
    assert(
        var_lookup.has_value() ^
        consts_lookup.has_value()); // xor - exactly one has to contain a value

    if (var_lookup.has_value()) {
        return {id->shallow_copy(), var_lookup.value().type};
    } else {
        TypeResult const_result =
            std::visit(*this, consts_lookup.value()->as_variant());
        std::shared_ptr<ASTNode> const_node(
            const_result.node); // prevents leaks, TODO
        return {id->shallow_copy(), const_result.type};
    }
}

TypeResult TypeChecker::operator()(ASTNodeArrAccess * arr) {
    auto lookup = _st->lookup_variable(arr->name);
    assert(lookup.has_value());

    type_ptr type = lookup->type;
    std::vector<std::shared_ptr<ASTNode>> new_idx;
    for (auto & idx : arr->idx_list) {
        if (!type_info::is_array_type(type)) {
            _errs.emplace_back("Cannot index a variable of non-array type: " +
                               arr->name);
        }
        std::shared_ptr<ArrayType> array_type = type_info::to_array_type(type);
        assert(array_type);

        TypeResult idx_res = std::visit(*this, idx->as_variant());
        if (!type_info::is_int(idx_res.type)) {
            _errs.emplace_back("Array index is not of int type: array name: " +
                               arr->name);
        }
        new_idx.emplace_back(new ASTNodeBinary(
            idx_res.node, new ASTNodeInt(array_type->normalizer),
            ASTNodeBinary::Operator::Add));
        type = array_type->elem_type;
    }

    return {new ASTNodeArrAccess(arr->name, new_idx), type};
}

TypeResult TypeChecker::operator()(ASTNodeAssign * assign) {
    TypeResult var = std::visit(*this, assign->target->as_variant());
    TypeResult rhs = std::visit(*this, assign->rhs->as_variant());
    if (!type_info::is_convertible(var.type, rhs.type)) {
        std::string var_id = type_info::get_type_identifier(var.type);
        std::string rhs_id = type_info::get_type_identifier(rhs.type);
        _errs.emplace_back("Cannot cast " + rhs_id + " to " + var_id);
    }

    ASTNode * rhs_node;
    if (!type_info::equal(var.type, rhs.type)) {
        rhs_node = new ASTNodeTypeCast(var.type, rhs.node);
    } else {
        rhs_node = rhs.node;
    }

    ASTNodeAssign * assign_node = assign->shallow_copy();
    auto * tgt = static_cast<ASTNodeAssignable *>(var.node);
    assert(tgt);
    assign_node->target = std::shared_ptr<ASTNodeAssignable>(tgt);
    assign_node->rhs = std::shared_ptr<ASTNode>(rhs_node);

    return {assign_node, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeCall * cnode) {
    auto fn_lookup = _st->lookup_function(cnode->fn);
    assert(fn_lookup.has_value());

    auto fn_args = fn_lookup.value().fn_type->args;
    auto call_arg_it = cnode->args.begin();
    assert(fn_args.size() == cnode->args.size());

    std::list<std::shared_ptr<ASTNode>> new_args;
    for (auto & arg : fn_args) {
        TypeResult call_arg = std::visit(*this, (*call_arg_it)->as_variant());
        if (!type_info::is_convertible(arg, call_arg.type)) {
            std::string call_arg_id =
                type_info::get_type_identifier(call_arg.type);
            std::string fn_arg_id = type_info::get_type_identifier(arg);
            _errs.emplace_back("Cannot cast " + call_arg_id + " to " +
                               fn_arg_id);
        }

        ASTNode * argument;
        if (!type_info::equal(arg, call_arg.type)) {
            argument = new ASTNodeTypeCast(arg, call_arg.node);
        } else {
            argument = call_arg.node;
        }
        new_args.emplace_back(argument);
        ++call_arg_it;
    }

    ASTNodeCall * call_node = cnode->shallow_copy();
    call_node->args = std::move(new_args);

    return {call_node, fn_lookup.value().fn_type->return_type};
}

TypeResult TypeChecker::operator()(ASTNodeBuiltinCall * cnode) {
    // TODO better overload checking
    auto lookup = _st->lookup_function(cnode->fn);
    assert(lookup.has_value());
    FunctionRecord fnr = lookup.value();

    std::string new_name = cnode->fn;
    std::list<std::shared_ptr<ASTNode>> new_args;
    auto f_arg_it = fnr.args.begin();
    auto c_arg_it = cnode->args.begin();
    assert(fnr.args.size() == cnode->args.size());
    for (; f_arg_it != fnr.args.end(); ++f_arg_it, ++c_arg_it) {
        TypeResult arg_res = std::visit(*this, (*c_arg_it)->as_variant());

        auto common_type =
            type_info::get_common_type((*f_arg_it).type, arg_res.type);

        if (!common_type) {
            _errs.emplace_back("No viable overload for builtin: " + cnode->fn);
            delete arg_res.node;
            break;
        }

        new_name += "_" + type_info::get_printable_id(common_type);
        new_args.emplace_back(arg_res.node);
    }

    return {new ASTNodeCall(new_name, new_args), fnr.return_type};
}

TypeResult TypeChecker::operator()(ASTNodeVarByRef * ref) {
    auto reffd_val = std::visit(*this, ref->var->as_variant());
    auto * var = static_cast<ASTNodeAssignable*>(reffd_val.node);
    type_ptr t(new RefType(reffd_val.type));
    return {new ASTNodeVarByRef(var), t};
}

TypeResult TypeChecker::operator()(ASTNodeFunction * fn) {
    std::shared_ptr<SymbolTable> old_st = _st;
    if (fn->proto->name() != "main") {
        auto fn_lookup = _st->lookup_function(fn->proto->name());
        assert(fn_lookup.has_value());

        FunctionRecord fnr = fn_lookup.value();
        old_st = _st;
        _st = fnr.symbol_table;
    }

    TypeResult block = std::visit(*this, fn->block->as_variant());
    TypeResult body = std::visit(*this, fn->body->as_variant());

    ASTNodeFunction * fn_node = fn->shallow_copy();
    fn_node->block = std::shared_ptr<ASTNode>(block.node);
    fn_node->body = std::shared_ptr<ASTNode>(body.node);

    _st = old_st;
    return {fn_node, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeIf * if_) {
    TypeResult cond = std::visit(*this, if_->cond->as_variant());
    TypeResult body = std::visit(*this, if_->body->as_variant());
    TypeResult else_branch;
    if (if_->else_.has_value()) {
        else_branch = std::visit(*this, if_->else_.value()->as_variant());
    }

    ASTNodeIf * if_node = if_->shallow_copy();
    if_node->cond = std::shared_ptr<ASTNode>(cond.node);
    if_node->body = std::shared_ptr<ASTNode>(body.node);
    if (else_branch.node) {
        if_node->else_ = std::shared_ptr<ASTNode>(else_branch.node);
    }

    return {if_node, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeWhile * wnode) {
    auto old_st = _st;
    _st = _st->derive();

    TypeResult cond = std::visit(*this, wnode->cond->as_variant());
    TypeResult body = std::visit(*this, wnode->body->as_variant());

    ASTNodeWhile * while_node = wnode->shallow_copy();
    while_node->cond = std::shared_ptr<ASTNode>(cond.node);
    while_node->body = std::shared_ptr<ASTNode>(body.node);

    _st = old_st;
    return {while_node, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeFor * fnode) {
    auto old_st = _st;
    _st = _st->derive();
    _st->variables[fnode->var] = {
        fnode->var, std::shared_ptr<Type>(_tf.get_int_t()), false};

    TypeResult body = std::visit(*this, fnode->body->as_variant());
    TypeResult it_start = std::visit(*this, fnode->it_start->as_variant());
    TypeResult it_stop = std::visit(*this, fnode->it_stop->as_variant());

    if (!type_info::is_int(it_start.type)) {
        _errs.emplace_back("Iteration init is not of type int.");
    }
    if (!type_info::is_int(it_stop.type)) {
        _errs.emplace_back("Iteration stop val is not of type int.");
    }

    _st = old_st;

    ASTNodeFor * for_node = fnode->shallow_copy();
    for_node->body = std::shared_ptr<ASTNode>(body.node);
    for_node->it_start = std::shared_ptr<ASTNode>(it_start.node);
    for_node->it_stop = std::shared_ptr<ASTNode>(it_stop.node);
    return {for_node, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeBody * bnode) {
    std::list<std::shared_ptr<ASTNode>> statements;
    for (auto & stmt : bnode->stmts) {
        TypeResult statement_res = std::visit(*this, stmt->as_variant());
        statements.emplace_back(statement_res.node);
    }

    ASTNodeBody * body = bnode->shallow_copy();
    body->stmts = std::move(statements);
    return {body, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeBlock * block_node) {
    std::list<std::shared_ptr<ASTNode>> new_decls;
    for (auto & block : block_node->decls) {
        TypeResult block_res = std::visit(*this, block->as_variant());
        new_decls.emplace_back(block_res.node);
    }

    return {new ASTNodeBlock(new_decls), nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeVar * vnode) {
    for (auto & decl : vnode->vars) {
        if (!_st->lookup_variable(decl.name)) {
            _st->variables[decl.name] = decl;
        }
    }
    return {vnode->shallow_copy(), nullptr};
}

TypeResult TypeChecker::operator()(ASTNode * node) {
    return {node->shallow_copy(), nullptr};
}
