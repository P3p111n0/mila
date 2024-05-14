#pragma once

#include "ASTNode.hpp"
#include "Type.hpp"
#include "VariableRecord.hpp"
#include <list>
#include <memory>
#include <optional>
#include <unordered_map>

struct SymbolTable;
using callsite_ptr = std::shared_ptr<std::vector<std::shared_ptr<ASTNodeCall>>>;
struct FunctionRecord {
    std::string name;
    std::shared_ptr<Type> return_type;
    std::list<VariableRecord> args;
    std::size_t arity;
    std::shared_ptr<SymbolTable> symbol_table;
    std::shared_ptr<FnType> fn_type;
    callsite_ptr callsites;
};

struct SymbolTable {
  public:
    enum class Scope { Global, Function, Loop, If };

    SymbolTable() : _parent(nullptr) {}
    std::shared_ptr<SymbolTable> derive();

    std::optional<FunctionRecord> lookup_function(const std::string &,
                                                  Scope = Scope::Global) const;
    std::optional<VariableRecord> lookup_variable(const std::string &,
                                                  Scope = Scope::Global) const;
    std::optional<std::shared_ptr<ASTNode>>
    lookup_constant(const std::string &, Scope = Scope::Global) const;
    void add_callsite(const std::string &, std::shared_ptr<ASTNodeCall>);
    void edit_function(const std::string &, FunctionRecord);

    bool unique_in_current_scope(const std::string &) const;
    bool unique_global(const std::string &) const;

    bool running_in_scope(Scope) const;

    static callsite_ptr make_callsite_ptr() {
        return std::make_shared<std::vector<std::shared_ptr<ASTNodeCall>>>();
    }

    std::unordered_map<std::string, FunctionRecord> functions;
    std::unordered_map<std::string, std::shared_ptr<ASTNode>> constants;
    std::unordered_map<std::string, VariableRecord> variables;

    Scope current_scope;

  private:
    SymbolTable * _parent;
};
