#pragma once

#include "VariableRecord.hpp"
#include "ASTNode.hpp"
#include <memory>
#include <unordered_map>
#include <list>
#include <optional>

struct SymbolTable;

struct FunctionRecord {
    std::string name;
    VarType return_type;
    std::list<VariableRecord> args;
    std::size_t arity;
    std::shared_ptr<SymbolTable> symbol_table;
};

struct SymbolTable {
  public:
    enum class Scope {
        Global,
        Function,
        Loop
    };

    SymbolTable() : _parent(nullptr) {}
    std::shared_ptr<SymbolTable> derive();

    std::optional<FunctionRecord> lookup_function(const std::string &) const;
    std::optional<VariableRecord> lookup_variable(const std::string &) const;
    std::optional<std::shared_ptr<ASTNode>> lookup_constant(const std::string &) const;

    bool unique_in_current_scope(const std::string &) const;
    bool unique_global(const std::string &) const;

    std::unordered_map<std::string, FunctionRecord> functions;
    std::unordered_map<std::string, std::shared_ptr<ASTNode>> constants;
    std::unordered_map<std::string, VariableRecord> variables;

    Scope current_scope;
  private:
    SymbolTable * _parent;
};
