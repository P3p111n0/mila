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
    return nullptr;
}

TypeResult TypeChecker::operator()(ASTNodeInt * node) {
    return {new ASTNodeInt(node->val), _tf.get_int_t()};
}

TypeResult TypeChecker::operator()(ASTNodeUnary * node) {
    TypeResult arg = std::visit(*this, node->arg->as_variant());
    ASTNode * new_node = new ASTNodeUnary(arg.node, node->op);
    return {new_node, arg.type};
}

TypeResult TypeChecker::operator()(ASTNodeBinary * binary_node) {
    TypeResult lhs_res = std::visit(*this, binary_node->lhs->as_variant());
    TypeResult rhs_res = std::visit(*this, binary_node->rhs->as_variant());

    Type * common_type = TypeInfo::get_common_type(lhs_res.type, rhs_res.type);
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
            new ASTNodeTypeCast(lhs_res.type, common_type, lhs_res.node);
    } else {
        final_lhs = lhs_res.node;
    }

    ASTNode * final_rhs;
    if (!TypeInfo::equal(common_type, rhs_res.type)) {
        final_rhs =
            new ASTNodeTypeCast(rhs_res.type, common_type, rhs_res.node);
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

    return {final_binary_node, common_type};
}

TypeResult TypeChecker::operator()(ASTNodeFBinary *) {
    assert(0 && "unreachable");
}

TypeResult TypeChecker::operator()(ASTNodeIdentifier * id) {
    auto lookup = _st->lookup_variable(id->name);
    assert(lookup.has_value());
    // TODO fix probable segfault
    return {new ASTNodeIdentifier(id->name), lookup.value().type.get()};
}

TypeResult TypeChecker::operator()(ASTNodeAssign * assign) {
    auto target_lookup = _st->lookup_variable(assign->target);
    assert(target_lookup.has_value());

    VariableRecord var = target_lookup.value();
    TypeResult rhs = std::visit(*this, assign->rhs->as_variant());
    if (!TypeInfo::is_convertible(var.type.get(), rhs.type)) {
        std::string var_id = TypeInfo::get_type_identifier(var.type.get());
        std::string rhs_id = TypeInfo::get_type_identifier(rhs.type);
        _errs.emplace_back("Cannot cast " + rhs_id + " to " + var_id);
        return {};
    }

    ASTNode * rhs_node;
    if (!TypeInfo::equal(var.type.get(), rhs.type)) {
        rhs_node = new ASTNodeTypeCast(rhs.type, var.type.get(), rhs.node);
    } else {
        rhs_node = rhs.node;
    }

    return {rhs_node, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeCall * cnode) {
    auto fn_lookup = _st->lookup_function(cnode->fn);
    assert(fn_lookup.has_value());

    auto call_arg_it = cnode->args.begin();
    std::vector<std::shared_ptr<Type>> fn_args = fn_lookup.value().fn_type->get_args();

    assert(fn_args.size() == cnode->args.size());

    std::list<std::shared_ptr<ASTNode>> new_args;
    for (auto & arg : fn_args) {
        TypeResult call_arg = std::visit(*this, (*call_arg_it)->as_variant());
        if (!TypeInfo::is_convertible(arg.get(), call_arg.type)) {
            std::string call_arg_id = TypeInfo::get_type_identifier(call_arg.type);
            std::string fn_arg_id = TypeInfo::get_type_identifier(arg.get());
            _errs.emplace_back("Cannot cast " + call_arg_id + " to " + fn_arg_id);
            new_args.emplace_back(nullptr);
            ++call_arg_it;
            continue;
        }

        ASTNode * argument;
        if (!TypeInfo::equal(arg.get(), call_arg.type)) {
            argument = new ASTNodeTypeCast(call_arg.type, arg.get(), call_arg.node);
        } else {
            argument = call_arg.node;
        }
        new_args.emplace_back(argument);
        ++call_arg_it;
    }

    ASTNode * call_node = new ASTNodeCall(cnode->fn, new_args);

    return {call_node, fn_lookup.value().fn_type->get_return_type()};
}

TypeResult TypeChecker::operator()(ASTNodeVarByRef * ref) {
    auto lookup = _st->lookup_variable(ref->var);
    assert(lookup.has_value());

    ASTNode * node = new ASTNodeVarByRef(ref->var);
    RefType * t = new RefType(lookup.value().type.get());
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

    ASTNodeFunction * fn_node =
        new ASTNodeFunction(fn->proto.get(), block.node, body.node);

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

    ASTNode * if_node;
    if (else_branch.node) {
        if_node = new ASTNodeIf(cond.node, body.node, else_branch.node);
    } else {
        if_node = new ASTNodeIf(cond.node, body.node);
    }

    return {if_node, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeWhile * wnode) {
    auto old_st = _st;
    _st = _st->derive();

    TypeResult cond = std::visit(*this, wnode->cond->as_variant());
    TypeResult body = std::visit(*this, wnode->body->as_variant());

    ASTNode * while_node = new ASTNodeWhile(cond.node, body.node);

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

    ASTNode * for_node = new ASTNodeFor(fnode->var, it_start.node, it_stop.node, body.node, fnode->is_downto);
    return {for_node, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeBody * bnode) {
    std::list<std::shared_ptr<ASTNode>> statements;
    for (auto & stmt : bnode->stmts) {
        TypeResult statement_res = std::visit(*this, stmt->as_variant());
        statements.emplace_back(statement_res.node);
    }

    ASTNode * body = new ASTNodeBody(statements);
    return {body, nullptr};
}

TypeResult TypeChecker::operator()(ASTNode * node) { return {node, nullptr}; }
