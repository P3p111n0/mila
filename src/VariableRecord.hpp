#pragma once

#include <string>

enum class VarType {
    Int,
    Void
};

struct VariableRecord {
    std::string name;
    VarType type;
    bool pass_by_ref = false;
};