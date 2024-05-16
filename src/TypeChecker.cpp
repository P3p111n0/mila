#include "TypeChecker.hpp"
#include "TypeInfo.hpp"

void TypeChecker::errs(std::ostream & os) const {
    for (const auto & err : _errs) {
        os << err << std::endl;
    }
}

std::shared_ptr<ASTNode> TypeChecker::tree_rebuild(ast_ptr main_node) {
    TypeResult main_node_tr = std::visit(*this, main_node->as_variant());
    auto result = std::shared_ptr<ASTNode>(main_node_tr.node);
    if (_errs.empty()) {
        return result;
    }
    return nullptr;
}

type_ptr TypeChecker::get_expr_type(ast_ptr expr) {
    TypeResult res = std::visit(*this, expr->as_variant());
    return res.type;
}

TypeResult TypeChecker::operator()(ASTNodeInt * node) {
    return {ast_ptr(node->shallow_copy()), type_ptr(_tf.get_int_t())};
}

TypeResult TypeChecker::operator()(ASTNodeString * str) {
    return {ast_ptr(str->shallow_copy()), type_ptr(_tf.get_string_t())};
}

TypeResult TypeChecker::operator()(ASTNodeDouble * dval) {
    return {ast_ptr(dval->shallow_copy()), type_ptr(_tf.get_double_t())};
}

TypeResult TypeChecker::operator()(ASTNodeUnary * node) {
    TypeResult arg = std::visit(*this, node->arg->as_variant());
    if (!type_info::is_int(arg.type)) {
        _errs.emplace_back("argument of unary operation has to be of type int");
    }
    ast_ptr new_node(new ASTNodeUnary(arg.node, node->op));
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

    ast_ptr final_lhs;
    if (!type_info::equal(common_type, lhs_res.type)) {
        final_lhs = ast_ptr(new ASTNodeTypeCast(common_type, lhs_res.node));
    } else {
        final_lhs = lhs_res.node;
    }

    ast_ptr final_rhs;
    if (!type_info::equal(common_type, rhs_res.type)) {
        final_rhs = ast_ptr(new ASTNodeTypeCast(common_type, rhs_res.node));
    } else {
        final_rhs = rhs_res.node;
    }

    ast_ptr final_binary_node;
    if (type_info::is_int(common_type)) {
        final_binary_node =
            ast_ptr(new ASTNodeBinary(final_lhs, final_rhs, binary_node->op));
    } else if (type_info::is_double(common_type)) {
        auto op = type_info::int_to_fp_arithmetic(binary_node->op);
        if (!op.has_value()) {
            _errs.emplace_back("operation not supported for fp type");
            final_binary_node = ast_ptr(new ASTNodeFBinary(
                final_lhs, final_rhs, ASTNodeFBinary::Operator(-1)));
        } else {
            final_binary_node =
                ast_ptr(new ASTNodeFBinary(final_lhs, final_rhs, op.value()));
        }
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
        return {ast_ptr(id->shallow_copy()), var_lookup.value().type};
    } else {
        TypeResult const_result =
            std::visit(*this, consts_lookup.value()->as_variant());
        return {ast_ptr(id->shallow_copy()), const_result.type};
    }
}

TypeResult TypeChecker::operator()(ASTNodeArrAccess * arr) {
    auto lookup = _st->lookup_variable(arr->name);
    assert(lookup.has_value());

    type_ptr type = lookup->type;
    std::vector<ast_ptr> new_idx;
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
            idx_res.node, ast_ptr(new ASTNodeInt(array_type->normalizer)),
            ASTNodeBinary::Operator::Add));
        type = array_type->elem_type;
    }

    return {ast_ptr(new ASTNodeArrAccess(arr->name, new_idx)), type};
}

TypeResult TypeChecker::operator()(ASTNodeAssign * assign) {
    TypeResult var = std::visit(*this, assign->target->as_variant());
    TypeResult rhs = std::visit(*this, assign->rhs->as_variant());
    if (!type_info::is_convertible(var.type, rhs.type)) {
        std::string var_id = type_info::get_type_identifier(var.type);
        std::string rhs_id = type_info::get_type_identifier(rhs.type);
        _errs.emplace_back("Cannot cast " + rhs_id + " to " + var_id);
    }

    ast_ptr rhs_node;
    if (!type_info::equal(var.type, rhs.type)) {
        rhs_node = ast_ptr(new ASTNodeTypeCast(var.type, rhs.node));
    } else {
        rhs_node = rhs.node;
    }

    std::shared_ptr<ASTNodeAssignable> tgt =
        std::dynamic_pointer_cast<ASTNodeAssignable>(var.node);
    assert(tgt);
    ast_ptr assign_node = ast_ptr(new ASTNodeAssign(tgt, rhs_node));

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

        ast_ptr argument;
        if (!type_info::equal(arg, call_arg.type)) {
            argument = ast_ptr(new ASTNodeTypeCast(arg, call_arg.node));
        } else {
            argument = call_arg.node;
        }
        new_args.emplace_back(argument);
        ++call_arg_it;
    }

    ast_ptr call_node = ast_ptr(new ASTNodeCall(cnode->fn, new_args));

    return {call_node, fn_lookup.value().fn_type->return_type};
}

TypeResult TypeChecker::operator()(ASTNodeBuiltinCall * cnode) {
    auto lookup = _st->lookup_function(cnode->fn);
    assert(lookup.has_value());
    FunctionRecord fnr = lookup.value();

    if (fnr.name == "to_int" || fnr.name == "to_double") {
        assert(cnode->args.size() == 1);
        type_ptr tgt = fnr.name == "to_int" ? type_ptr(_tf.get_int_t())
                                            : type_ptr(_tf.get_double_t());
        std::shared_ptr<ASTNode> arg = *(cnode->args.begin());
        TypeResult to_cast = std::visit(*this, arg->as_variant());
        if (!type_info::is_convertible(to_cast.type, tgt)) {
            auto src_type_id = type_info::get_type_identifier(to_cast.type);
            auto tgt_type_id = type_info::get_type_identifier(tgt);
            _errs.emplace_back("Cannot cast type " + src_type_id + " to type " +
                               tgt_type_id);
        }
        return {ast_ptr(new ASTNodeTypeCast(tgt, to_cast.node)), tgt};
    }

    std::string new_name = cnode->fn;
    std::list<ast_ptr> new_args;
    auto f_arg_it = fnr.args.begin();
    auto c_arg_it = cnode->args.begin();
    assert(fnr.args.size() == cnode->args.size());
    for (; f_arg_it != fnr.args.end(); ++f_arg_it, ++c_arg_it) {
        TypeResult arg_res = std::visit(*this, (*c_arg_it)->as_variant());

        auto common_type =
            type_info::get_common_type((*f_arg_it).type, arg_res.type);

        if (!common_type) {
            _errs.emplace_back("No viable overload for builtin: " + cnode->fn);
        }

        if (!type_info::equal(arg_res.type, common_type)) {
            arg_res.node =
                ast_ptr(new ASTNodeTypeCast(common_type, arg_res.node));
        }

        new_name += "_" + type_info::get_printable_id(common_type);
        new_args.emplace_back(arg_res.node);
    }

    return {ast_ptr(new ASTNodeCall(new_name, new_args)), fnr.return_type};
}

TypeResult TypeChecker::operator()(ASTNodeVarByRef * ref) {
    auto reffd_val = std::visit(*this, ref->var->as_variant());
    std::shared_ptr<ASTNodeAssignable> var =
        std::dynamic_pointer_cast<ASTNodeAssignable>(reffd_val.node);
    assert(var);
    type_ptr t(new RefType(reffd_val.type));
    return {ast_ptr(new ASTNodeVarByRef(var)), t};
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

    ast_ptr fn_node =
        ast_ptr(new ASTNodeFunction(fn->proto, block.node, body.node));

    _st = old_st;
    return {fn_node, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeIf * if_) {
    TypeResult cond = std::visit(*this, if_->cond->as_variant());

    auto old_st = _st;
    _st = _st->derive();

    TypeResult body = std::visit(*this, if_->body->as_variant());
    TypeResult else_branch;
    if (if_->else_.has_value()) {
        else_branch = std::visit(*this, if_->else_.value()->as_variant());
    }

    _st = old_st;

    std::shared_ptr<ASTNodeIf> if_node(new ASTNodeIf(cond.node, body.node));
    if (else_branch.node) {
        if_node->else_ = else_branch.node;
    }

    return {if_node, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeWhile * wnode) {
    auto old_st = _st;
    _st = _st->derive();

    TypeResult cond = std::visit(*this, wnode->cond->as_variant());
    TypeResult body = std::visit(*this, wnode->body->as_variant());

    ast_ptr while_node(new ASTNodeWhile(cond.node, body.node));

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

    ast_ptr for_node(new ASTNodeFor(fnode->var, it_start.node, it_stop.node,
                                    body.node, fnode->is_downto));
    return {for_node, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeBody * bnode) {
    std::list<std::shared_ptr<ASTNode>> statements;
    for (auto & stmt : bnode->stmts) {
        TypeResult statement_res = std::visit(*this, stmt->as_variant());
        statements.emplace_back(statement_res.node);
    }

    ast_ptr body(new ASTNodeBody(statements));
    return {body, nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeBlock * block_node) {
    std::list<std::shared_ptr<ASTNode>> new_decls;
    for (auto & block : block_node->decls) {
        TypeResult block_res = std::visit(*this, block->as_variant());
        new_decls.emplace_back(block_res.node);
    }

    return {ast_ptr(new ASTNodeBlock(new_decls)), nullptr};
}

TypeResult TypeChecker::operator()(ASTNodeVar * vnode) {
    for (auto & decl : vnode->vars) {
        if (!_st->variables.contains(decl.name)) {
            _st->variables[decl.name] = decl;
        }
    }
    return {ast_ptr(vnode->shallow_copy()), nullptr};
}

TypeResult TypeChecker::operator()(ASTNode * node) {
    return {ast_ptr(node->shallow_copy()), nullptr};
}
