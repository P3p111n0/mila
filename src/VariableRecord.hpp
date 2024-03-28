#pragma once

#include <string>

enum class Type {
    Int,
    Void
};

struct VariableRecord {
    std::string name;
    Type type;
};