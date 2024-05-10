#pragma once

#include "Type.hpp"

class BaseTypeFactory {
  public:
    BaseTypeFactory() = default;

    BaseType * get_int_t() const {
        return new BaseType(BaseType::Builtin::Int);
    };
    BaseType * get_double_t() const {
        return new BaseType(BaseType::Builtin::Double);
    };
    BaseType * get_string_t() const {
        return new BaseType(BaseType::Builtin::String);
    };
    BaseType * get_void_t() const {
        return new BaseType(BaseType::Builtin::Void);
    };
};