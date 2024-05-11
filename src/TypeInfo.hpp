#pragma once

#include "Type.hpp"
#include "BaseTypeFactory.hpp"

namespace type_info {

bool is_int(type_ptr);

bool is_double(type_ptr);

bool is_string(type_ptr);

bool is_void(type_ptr);

bool is_base_type(type_ptr);

bool is_ref_type(type_ptr);

bool is_mimic_type(type_ptr);

bool is_array_type(type_ptr);

BaseType * to_base_type(Type *);

std::shared_ptr<BaseType> to_base_type(type_ptr);

RefType * to_ref_type(Type *);

std::shared_ptr<RefType> to_ref_type(type_ptr);

ArrayType * to_array_type(Type *);

std::shared_ptr<ArrayType> to_array_type(type_ptr);

bool equal(type_ptr, type_ptr);

bool is_convertible(type_ptr, type_ptr);

type_ptr get_common_type(type_ptr, type_ptr);

std::string get_type_identifier(type_ptr);

std::string get_printable_id(type_ptr);

}