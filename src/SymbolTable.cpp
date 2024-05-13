#include "SymbolTable.hpp"

std::shared_ptr<SymbolTable> SymbolTable::derive() {
    auto child = std::make_shared<SymbolTable>();
    child->_parent = this;
    return child;
}

std::optional<std::shared_ptr<ASTNode>>
SymbolTable::lookup_constant(const std::string & name, Scope top) const {
    if (constants.count(name)) {
        return constants.at(name);
    }
    if (_parent && current_scope != top) {
        return _parent->lookup_constant(name);
    }
    return std::nullopt;
}

std::optional<FunctionRecord>
SymbolTable::lookup_function(const std::string & name, Scope top) const {
    if (functions.count(name)) {
        return functions.at(name);
    }
    if (_parent && current_scope != top) {
        return _parent->lookup_function(name);
    }
    return std::nullopt;
}

std::optional<VariableRecord>
SymbolTable::lookup_variable(const std::string & name, Scope top) const {
    if (variables.count(name)) {
        return variables.at(name);
    }
    if (_parent && current_scope != top) {
        return _parent->lookup_variable(name);
    }
    return std::nullopt;
}

bool SymbolTable::unique_in_current_scope(const std::string & name) const {
    return !(constants.count(name) || variables.count(name) ||
             functions.count(name));
}

bool SymbolTable::unique_global(const std::string & name) const {
    auto fn = lookup_function(name);
    auto var = lookup_variable(name);
    auto cst = lookup_constant(name);
    return !(fn.has_value() || var.has_value() || cst.has_value());
}

void SymbolTable::add_callsite(const std::string & fn_name,
                               std::shared_ptr<ASTNodeCall> node) {
    if (functions.contains(fn_name)) {
        functions[fn_name].callsites.emplace_back(node);
    } else {
        if (_parent) {
            _parent->add_callsite(fn_name, node);
        }
        return;
    }
}

void SymbolTable::edit_function(const std::string & fn, FunctionRecord record) {
    if (functions.contains(fn)) {
        functions[fn] = std::move(record);
        return;
    }
    if (_parent) {
        _parent->edit_function(fn, std::move(record));
    }
}