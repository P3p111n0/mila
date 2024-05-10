#include "TypeChecker.hpp"
#include "TypeInfo.hpp"

void TypeChecker::errs(std::ostream & os) const {
    for (const auto & err : _errs) {
        os << err << std::endl;
    }
}

ASTNode * TypeChecker::tree_rebuild(ASTNode * main_node) {
    TypeResult res = std::visit(*this, main_node->as_variant());
    if (_errs.empty()) {
        return res.node;
    }
    delete res.node;
    return nullptr;
}

TypeResult TypeChecker::operator()(ASTNodeInt * node) {
    return {node->shallow_copy(), type_ptr(_tf.get_int_t())};
}

TypeResult TypeChecker::operator()(ASTNodeUnary * node) {
    TypeResult arg = std::visit(*this, node->arg->as_variant());
    ASTNodeUnary * new_node = node->shallow_copy();
    new_node->arg = std::shared_ptr<ASTNode>(arg.node);
    return {new_node, arg.type};
}

TypeResult TypeChecker::operator()(ASTNodeBinary * binary_node) {
    TypeResult lhs_res = std::visit(*this, binary_node->lhs->as_variant());
    TypeResult rhs_res = std::visit(*this, binary_node->rhs->as_variant());

    type_ptr common_type = TypeInfo::get_common_type(lhs_res.type, rhs_res.type);
    if (!common_type) {
        std::string lhs_id = TypeInfo::get_type_identifier(lhs_res.type);
        std::string rhs_id = TypeInfo::get_type_identifier(rhs_res.type);
        _errs.emplace_back("Cannot convert " + lhs_id + " and " + rhs_id +
                           " to a common type.");
        return {};
    }

    ASTNode * final_lhs;
    if (!TypeInfo::equal(common_type, lhs_res.type)) {
        final_lhs =
            new ASTNodeTypeCast(common_type, lhs_res.node);
    } else {
        final_lhs = lhs_res.node;
    }

    ASTNode * final_rhs;
    if (!TypeInfo::equal(common_type, rhs_res.type)) {
        final_rhs =
            new ASTNodeTypeCast(common_type, rhs_res.node);
    } else {
        final_rhs = rhs_res.node;
    }

    ASTNode * final_binary_node;
    if (TypeInfo::is_int(common_type)) {
        final_binary_node =
            new ASTNodeBinary(final_lhs, final_rhs, binary_node->op);
    } else if (TypeInfo::is_double(common_type)) {
        // TODO operation resolution
        final_binary_node = new ASTNodeFBinary(final_lhs, final_rhs,
                                               ASTNodeFBinary::Operator(-1));
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

TypeResult TypeChecker::operator()(ASTNodeAssign * assign) {
    auto target_lookup = _st->lookup_variable(assign->target);
    assert(target_lookup.has_value());

    VariableRecord var = target_lookup.value();
    TypeResult rhs = std::visit(*this, assign->rhs->as_variant());
    if (!TypeInfo::is_convertible(var.type, rhs.type)) {
        std::string var_id = TypeInfo::get_type_identifier(var.type);
        std::string rhs_id = TypeInfo::get_type_identifier(rhs.type);
        _errs.emplace_back("Cannot cast " + rhs_id + " to " + var_id);
    }

    ASTNode * rhs_node;
    if (!TypeInfo::equal(var.type, rhs.type)) {
        rhs_node =
            new ASTNodeTypeCast(var.type, rhs.node);
    } else {
        rhs_node = rhs.node;
    }

    ASTNodeAssign * assign_node = assign->shallow_copy();
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
        if (!TypeInfo::is_convertible(arg, call_arg.type)) {
            std::string call_arg_id =
                TypeInfo::get_type_identifier(call_arg.type);
            std::string fn_arg_id = TypeInfo::get_type_identifier(arg);
            _errs.emplace_back("Cannot cast " + call_arg_id + " to " +
                               fn_arg_id);
            new_args.emplace_back(nullptr);
            ++call_arg_it;
            continue;
        }

        ASTNode * argument;
        if (!TypeInfo::equal(arg, call_arg.type)) {
            argument = new ASTNodeTypeCast(arg,
                                           call_arg.node);
        } else {
            argument = call_arg.node;
        }
        new_args.emplace_back(argument);
        ++call_arg_it;
    }

    ASTNodeCall * call_node = cnode->shallow_copy();
    call_node->args = std::move(new_args);

    return {call_node,
            fn_lookup.value().fn_type->return_type};
}

TypeResult TypeChecker::operator()(ASTNodeBuiltinCall * cnode) {
    //TODO better overload checking
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

        auto common_type = TypeInfo::get_common_type((*f_arg_it).type, arg_res.type);

        if (!common_type) {
            _errs.emplace_back("No viable overload for builtin: " + cnode->fn);
            delete arg_res.node;
            break;
        }

        new_name += "_" + TypeInfo::get_printable_id(common_type);
        new_args.emplace_back(arg_res.node);
    }

    return {new ASTNodeCall(new_name, new_args), fnr.return_type};
}

TypeResult TypeChecker::operator()(ASTNodeVarByRef * ref) {
    auto lookup = _st->lookup_variable(ref->var);
    assert(lookup.has_value());

    ASTNode * node = ref->shallow_copy();
    type_ptr t(new RefType(lookup.value().type));
    return {node, t};
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

    if (!TypeInfo::is_int(it_start.type)) {
        _errs.emplace_back("Iteration init is not of type int.");
    }
    if (!TypeInfo::is_int(it_stop.type)) {
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

TypeResult TypeChecker::operator()(ASTNodeBlock * block) {
    std::list<std::shared_ptr<ASTNode>> new_decls;
    for (auto & block : block->decls) {
        TypeResult block_res = std::visit(*this, block->as_variant());
        new_decls.emplace_back(block_res.node);
    }

    return {new ASTNodeBlock(new_decls), nullptr};
}

TypeResult TypeChecker::operator()(ASTNode * node) {
    return {node->shallow_copy(), nullptr};
}
