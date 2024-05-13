#include "SymbolTable.hpp"

std::shared_ptr<SymbolTable> SymbolTable::derive() {
    auto child = std::make_shared<SymbolTable>();
    child->_parent = this;
    return child;
}

std::optional<std::shared_ptr<ASTNode>> SymbolTable::lookup_constant(const std::string & name) const {
    if (constants.count(name)) {
        return constants.at(name);
    }
    if (_parent) {
        return _parent->lookup_constant(name);
    }
    return std::nullopt;
}

std::optional<FunctionRecord> SymbolTable::lookup_function(const std::string & name) const {
    if (functions.count(name)) {
        return functions.at(name);
    }
    if (_parent) {
        return _parent->lookup_function(name);
    }
    return std::nullopt;
}

std::optional<VariableRecord> SymbolTable::lookup_variable(const std::string & name) const {
    if (variables.count(name)) {
        return variables.at(name);
    }
    if (_parent) {
        return _parent->lookup_variable(name);
    }
    return std::nullopt;
}

bool SymbolTable::unique_in_current_scope(const std::string & name) const {
    return !(constants.count(name) || variables.count(name) || functions.count(name));
}

bool SymbolTable::unique_global(const std::string & name) const {
    auto fn = lookup_function(name);
    auto var = lookup_variable(name);
    auto cst = lookup_constant(name);
    return !(fn.has_value() || var.has_value() || cst.has_value());
}

void SymbolTable::add_callsite(const std::string & fn_name, ASTNodeCall * node) {
    if (functions.contains(fn_name)) {
        functions[fn_name].callsites.emplace_back(node);
    } else {
        if (_parent) {
            _parent->add_callsite(fn_name, node);
        }
        return;
    }
}