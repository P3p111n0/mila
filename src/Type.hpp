#pragma once

#include <memory>
#include <string>
#include <variant>
#include <vector>

class Type;
class BaseType;
class RefType;
class FnType;
class ArrayType;
class MimicType;

using type_ptr = std::shared_ptr<Type>;
using TypeVariant =
    std::variant<BaseType *, RefType *, FnType *, ArrayType *, MimicType *>;

class Type {
  public:
    Type() = default;
    virtual ~Type() = default;
    virtual TypeVariant as_variant() = 0;
};

class BaseType : public Type {
  public:
    enum class Builtin { Int, Double, String, Void };
    explicit BaseType(Builtin id) : id(std::move(id)) {}
    TypeVariant as_variant() override { return this; };

    Builtin id;
};

class RefType : public Type {
  public:
    explicit RefType(Type * t) : base(t) {}
    explicit RefType(type_ptr t) : base(t) {}
    TypeVariant as_variant() override { return this; };

    std::shared_ptr<Type> base;
};

class FnType : public Type {
  public:
    FnType(std::vector<std::shared_ptr<Type>> args, std::shared_ptr<Type> rtype)
        : args(std::move(args)), return_type(rtype) {}
    TypeVariant as_variant() override { return this; };

    std::vector<std::shared_ptr<Type>> args;
    std::shared_ptr<Type> return_type;
};

class ArrayType : public Type {
  public:
    explicit ArrayType(type_ptr t, int lb, int ub)
        : elem_type(t), lower_bound(lb), upper_bound(ub) {
        normalizer = -lower_bound;
    }
    explicit ArrayType(type_ptr t) : elem_type(t) {}
    TypeVariant as_variant() override { return this; };
    int size() const {
        return upper_bound - lower_bound + 1;
    }

    std::shared_ptr<Type> elem_type;
    int lower_bound;
    int upper_bound;
    int normalizer;
};

class MimicType : public Type {
  public:
    MimicType(std::vector<type_ptr> mimicted_types)
        : mimed_types(std::move(mimicted_types)) {}
    TypeVariant as_variant() override { return this; }

    std::vector<type_ptr> mimed_types;
};