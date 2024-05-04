#pragma once

#include "Type.hpp"

class BaseTypeFactory {
  public:
    BaseTypeFactory() = default;

    BaseType * get_int_t() const {
        return new BaseType("int");
    };
    BaseType * get_double_t() const {
        return new BaseType("double");
    };
    BaseType * get_string_t() const {
        return new BaseType("string");
    };
    BaseType * get_void_t() const {
        return new BaseType("void");
    };
};