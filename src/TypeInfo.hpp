#pragma once

#include "Type.hpp"
#include "BaseTypeFactory.hpp"

class TypeInfo {
  public:
    static bool is_int(Type *);
    static bool is_double(Type *);
    static bool is_string(Type *);
    static bool is_void(Type *);
    static bool is_base_type(Type *);
    static bool is_ref_type(Type *);

    static BaseType * to_base_type(Type *);
    static std::shared_ptr<BaseType> to_base_type(type_ptr);
    static RefType * to_ref_type(Type *);
    static std::shared_ptr<RefType> to_ref_type(type_ptr);

    static bool equal(Type *, Type *);

    static bool is_convertible(Type *, Type *);
    static Type * get_common_type(Type *, Type*);
    static std::string get_type_identifier(Type *);
};