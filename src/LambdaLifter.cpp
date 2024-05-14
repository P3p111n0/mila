#include "LambdaLifter.hpp"
#include "BaseTypeFactory.hpp"
#include "TypeInfo.hpp"

void LambdaLifter::lift_tree(std::shared_ptr<ASTNode> root) {
    std::visit(*this, root->as_variant());
}

void LambdaLifter::lift(std::shared_ptr<ASTNodeAssignable> node) {
    assert(!_parent_fn.empty());
    ASTNodeFunction * fn = _parent_fn.top();
    if (fn->proto->name() == "main") {
        return;
    }
    std::optional<FunctionRecord> function_lookup =
        _st->lookup_function(fn->proto->name());
    assert(function_lookup.has_value());

    FunctionRecord fnr = function_lookup.value();
    auto & function_args = fn->proto->args;
    std::optional<VariableRecord> var_lookup = _st->lookup_variable(node->name);
    assert(var_lookup.has_value());

    VariableRecord var = var_lookup.value();
    type_ptr old_type = var.type;
    var.type = type_ptr(new RefType(old_type));
    function_args.emplace_back(var);
    fnr.fn_type->args.emplace_back(var.type);
    fnr.args.emplace_back(var);
    fnr.arity++;
    _st->edit_function(fnr.name, fnr);

    std::shared_ptr<ASTNodeVarByRef> call_arg(new ASTNodeVarByRef(nullptr));
    call_arg->var = node;

    for (auto call_site : *fnr.callsites) {
        call_site->args.emplace_back(call_arg);
    }

    var.type = old_type;
    fnr.symbol_table->variables[var.name] = std::move(var);
}

void LambdaLifter::lift_rename(ASTNodePrototype * proto) {
    assert(!_parent_fn.empty());
    std::string prefix = _parent_fn.top()->proto->fn_name;
    std::string old_name = proto->fn_name;
    assert(_st->functions.contains(old_name));
    FunctionRecord & fnr = _st->functions[old_name];

    std::string new_name = prefix + "-" + old_name;
    proto->fn_name = new_name;
    fnr.name = new_name;
    for (auto & callsite : *fnr.callsites) {
        callsite->fn = new_name;
    }

    if (_st->functions.contains(new_name)) {
        return;
    }

    _st->functions[new_name] = fnr;
    //_st->functions.erase(old_name);
    _fn_names[old_name] = new_name;
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

void LambdaLifter::operator()(ASTNodePrototype * proto) {
    if (proto->fn_name == "main") {
        return;
    }
    std::string old_name = proto->fn_name;
    lift_rename(proto);
    auto lookup = _st->lookup_function(proto->name());
    assert(lookup.has_value());
    FunctionRecord fnr = lookup.value();

    if (proto->is_forward_declared) {
        return;
    }

    _st->functions.erase(old_name);
    _st = fnr.symbol_table;
    _st->current_scope = SymbolTable::Scope::Function;

    if (!type_info::is_void(fnr.return_type)) {
        assert(_st->variables.contains(old_name) &&
               !_st->variables.contains(proto->fn_name));
        _st->variables[proto->fn_name] = _st->variables[old_name];
        _st->variables.erase(old_name);
    }
}

void LambdaLifter::operator()(ASTNodeFunction * fn) {
    auto old_st = _st;
    std::string old_name = fn->proto->fn_name;
    std::visit(*this, fn->proto->as_variant());

    _parent_fn.push(fn);
    std::visit(*this, fn->block->as_variant());
    std::visit(*this, fn->body->as_variant());
    _parent_fn.pop();

    if (_fn_names.contains(old_name)) {
        _fn_names.erase(old_name);
    }

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
    if (_fn_names.contains(id->name)) { // id is a return value
        id->name = _fn_names[id->name];
    }
    auto var_lookup =
        _st->lookup_variable(id->name, SymbolTable::Scope::Function);
    auto const_lookup = _st->lookup_constant(id->name);

    if (const_lookup.has_value()) {
        return;
    }

    if (!var_lookup.has_value()) {
        std::shared_ptr<ASTNodeAssignable> to_lift(id->shallow_copy());
        lift(to_lift);
    }
}

void LambdaLifter::operator()(ASTNodeArrAccess * arr) {
    if (_fn_names.contains(arr->name)) { // name references a return value
        arr->name = _fn_names[arr->name];
    }
    auto lookup = _st->lookup_variable(arr->name, SymbolTable::Scope::Function);
    if (!lookup.has_value()) {
        std::shared_ptr<ASTNodeAssignable> to_lift(arr->shallow_copy());
        lift(to_lift);
    }
    for (auto & idx : arr->idx_list) {
        std::visit(*this, idx->as_variant());
    }
}
