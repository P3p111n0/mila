#pragma once

#include <string>

enum class VarType {
    Int,
    Void
};

struct VariableRecord {
    std::string name;
    VarType type;
};