#pragma once

#include "Type.hpp"
#include <string>

struct VariableRecord {
    std::string name;
    std::shared_ptr<Type> type;
    bool pass_by_ref = false;
};