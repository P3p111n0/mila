#pragma once

#include "Type.hpp"

class TypeInfo {
  public:
    static bool is_int(Type *);
    static bool is_double(Type *);
    static bool is_string(Type *);
    static bool is_void(Type *);
};