#pragma once

#include "Type.hpp"
#include "BaseTypeFactory.hpp"

class TypeInfo {
  public:
    static bool is_int(type_ptr);
    static bool is_double(type_ptr);
    static bool is_string(type_ptr);
    static bool is_void(type_ptr);
    static bool is_base_type(type_ptr);
    static bool is_ref_type(type_ptr);
    static bool is_mimic_type(type_ptr);

    static BaseType * to_base_type(Type *);
    static std::shared_ptr<BaseType> to_base_type(type_ptr);
    static RefType * to_ref_type(Type *);
    static std::shared_ptr<RefType> to_ref_type(type_ptr);

    static bool equal(type_ptr, type_ptr);

    static bool is_convertible(type_ptr, type_ptr);
    static type_ptr get_common_type(type_ptr, type_ptr);
    static std::string get_type_identifier(type_ptr);
    static std::string get_printable_id(type_ptr);
};