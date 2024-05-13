#include "LambdaLifter.hpp"
#include "BaseTypeFactory.hpp"

void LambdaLifter::lift_tree(std::shared_ptr<ASTNode> root) {
    std::visit(*this, root->as_variant());
}

void LambdaLifter::lift(std::shared_ptr<ASTNodeAssignable> node) {
    assert(!_parent_fn.empty());
    ASTNodeFunction * fn = _parent_fn.top();
    if (fn->proto->name() == "main") {
        return;
    }
    std::optional<FunctionRecord> function_lookup = _st->lookup_function(fn->proto->name());
    assert(function_lookup.has_value());

    FunctionRecord fnr = function_lookup.value();
    auto & function_args = fn->proto->args;
    std::optional<VariableRecord> var_lookup = _st->lookup_variable(node->name);
    assert(var_lookup.has_value());

    VariableRecord var = var_lookup.value();
    var.type = type_ptr(new RefType(var.type));
    function_args.emplace_back(var);

    std::shared_ptr<ASTNodeVarByRef> call_arg(new ASTNodeVarByRef(nullptr));
    call_arg->var = node;

    for (auto call_site : fnr.callsites) {
        call_site->args.emplace_back(call_arg);
    }
    fnr.symbol_table->variables[var.name] = std::move(var);
}

void LambdaLifter::operator()(ASTNode *) {}

void LambdaLifter::operator()(ASTNodeBlock * block) {
    for (auto & b : block->decls) {
        std::visit(*this, b->as_variant());
    }
}

void LambdaLifter::operator()(ASTNodeBody * body) {
    for (auto & stmt : body->stmts) {
        std::visit(*this, stmt->as_variant());
    }
}

void LambdaLifter::operator()(ASTNodeVar * var) {
    for (auto & record : var->vars) {
        if (!_st->variables.contains(record.name)) {
            _st->variables[record.name] = record;
        }
    }
}

void LambdaLifter::operator()(ASTNodeFunction * fn) {
    auto old_st = _st;
    if (fn->proto->name() != "main") {
        auto lookup = _st->lookup_function(fn->proto->name());
        assert(lookup.has_value());
        FunctionRecord fnr = lookup.value();

        _st = fnr.symbol_table;
        _st->current_scope = SymbolTable::Scope::Function;
    }

    _parent_fn.push(fn);
    std::visit(*this, fn->block->as_variant());
    std::visit(*this, fn->body->as_variant());
    _parent_fn.pop();

    _st = old_st;
}

void LambdaLifter::operator()(ASTNodeWhile * while_node) {
    auto old_st = _st;
    _st = _st->derive();
    _st->current_scope = SymbolTable::Scope::Loop;

    std::visit(*this, while_node->cond->as_variant());
    std::visit(*this, while_node->body->as_variant());

    _st = old_st;
}

void LambdaLifter::operator()(ASTNodeFor * for_node) {
    auto old_st = _st;
    _st = _st->derive();
    _st->current_scope = SymbolTable::Scope::Loop;

    BaseTypeFactory btf;
    _st->variables[for_node->var] = {for_node->var, type_ptr(btf.get_int_t()),
                                     false};
    std::visit(*this, for_node->body->as_variant());

    _st = old_st;
}

void LambdaLifter::operator()(ASTNodeVarByRef * ref) {
    std::visit(*this, ref->var->as_variant());
}

void LambdaLifter::operator()(ASTNodeCall * call) {
    for (auto & arg : call->args) {
        std::visit(*this, arg->as_variant());
    }
}

void LambdaLifter::operator()(ASTNodeBinary * binary) {
    std::visit(*this, binary->lhs->as_variant());
    std::visit(*this, binary->rhs->as_variant());
}

void LambdaLifter::operator()(ASTNodeUnary * unary) {
    std::visit(*this, unary->arg->as_variant());
}

void LambdaLifter::operator()(ASTNodeAssign * assign) {
    std::visit(*this, assign->target->as_variant());
    std::visit(*this, assign->rhs->as_variant());
}

void LambdaLifter::operator()(ASTNodeIf * if_node) {
    std::visit(*this, if_node->cond->as_variant());

    auto old_st = _st;
    _st = _st->derive();
    _st->current_scope = SymbolTable::Scope::If;
    std::visit(*this, if_node->body->as_variant());
    if (if_node->else_.has_value()) {
        std::visit(*this, if_node->else_.value()->as_variant());
    }
    _st = old_st;
}

void LambdaLifter::operator()(ASTNodeIdentifier * id) {
    auto var_lookup = _st->lookup_variable(id->name, SymbolTable::Scope::Function);
    auto const_lookup = _st->lookup_constant(id->name);
    assert(var_lookup.has_value() ^ const_lookup.has_value());

    if (const_lookup.has_value()) {
        return;
    }

    if (!var_lookup.has_value()) {
        std::shared_ptr<ASTNodeAssignable> to_lift(id->shallow_copy());
        lift(to_lift);
    }
}

void LambdaLifter::operator()(ASTNodeArrAccess * arr) {
    auto lookup = _st->lookup_variable(arr->name, SymbolTable::Scope::Function);
    if (!lookup.has_value()) {
        std::shared_ptr<ASTNodeAssignable> to_lift(arr->shallow_copy());
        lift(to_lift);
    }
    for (auto & idx : arr->idx_list) {
        std::visit(*this, idx->as_variant());
    }
}
