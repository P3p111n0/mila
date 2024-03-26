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
