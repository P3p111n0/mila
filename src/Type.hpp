#pragma once

#include <memory>
#include <vector>
#include <string>
#include <variant>

class Type;
class BaseType;
class RefType;
class FnType;
class ArrayType;

using type_ptr = std::shared_ptr<Type>;
using TypeVariant = std::variant<BaseType*, RefType*, FnType*, ArrayType*>;

class Type {
  public:
    Type() = default;
    virtual ~Type() = default;
    virtual TypeVariant as_variant() = 0;
    virtual Type * shallow_copy() const = 0;
};

class BaseType : public Type {
  public:
    enum class Builtin {
        Int,
        Double,
        String,
        Void
    };
    explicit BaseType(Builtin id) : id(std::move(id)) {}
    TypeVariant as_variant() override {
        return this;
    };
    BaseType * shallow_copy() const override {
        return new BaseType(*this);
    }

    Builtin id;
};

class RefType : public Type {
  public:
    explicit RefType(Type * t) : base(t) {}
    explicit RefType(type_ptr t) : base(t) {}
    TypeVariant as_variant() override {
        return this;
    };
    RefType * shallow_copy() const override {
        return new RefType(*this);
    }

    std::shared_ptr<Type> base;
};

class FnType : public Type {
  public:
    FnType(std::vector<std::shared_ptr<Type>> args, std::shared_ptr<Type> rtype)
        : args(std::move(args)), return_type(rtype) {}
    TypeVariant as_variant() override {
        return this;
    };
    FnType * shallow_copy() const override {
        return new FnType(*this);
    }

    std::vector<std::shared_ptr<Type>> args;
    std::shared_ptr<Type> return_type;
};

class ArrayType : public Type {
  public:
    explicit ArrayType(Type * t) : elem_type(t) {}
    explicit ArrayType(type_ptr t) : elem_type(t) {}
    TypeVariant as_variant() override {
        return this;
    };
    ArrayType * shallow_copy() const override {
        return new ArrayType(*this);
    }

    std::shared_ptr<Type> elem_type;
};