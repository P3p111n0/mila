#pragma once

#include "Type.hpp"
#include "BaseTypeFactory.hpp"

class TypeInfo {
  public:
    static bool is_int(Type *);
    static bool is_double(Type *);
    static bool is_string(Type *);
    static bool is_void(Type *);

    static BaseType * to_base_type(Type *);

    static bool equal(Type *, Type *);

    static bool is_convertible(Type *, Type *);
    static Type * get_common_type(Type *, Type*);
    static std::string get_type_identifier(Type *);
};