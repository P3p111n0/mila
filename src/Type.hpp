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
    explicit BaseType(Builtin id) : _id(std::move(id)) {}
    Builtin id() const { return _id; }
    TypeVariant as_variant() override {
        return this;
    };
    BaseType * shallow_copy() const override {
        return new BaseType(*this);
    }

  private:
    Builtin _id;
};

class RefType : public Type {
  public:
    explicit RefType(Type * t) : _base(t) {}
    Type * get_referenced_type() const { return _base.get(); }
    TypeVariant as_variant() override {
        return this;
    };
    RefType * shallow_copy() const override {
        return new RefType(*this);
    }

  private:
    std::shared_ptr<Type> _base;
};

class FnType : public Type {
  public:
    FnType(std::vector<std::shared_ptr<Type>> args, std::shared_ptr<Type> rtype)
        : _args(std::move(args)), _return_type(rtype) {}
    TypeVariant as_variant() override {
        return this;
    };
    FnType * shallow_copy() const override {
        return new FnType(*this);
    }

    std::vector<std::shared_ptr<Type>> get_args() const {
        return _args;
    }

    Type * get_return_type() const {
        return _return_type.get();
    }
  private:
    std::vector<std::shared_ptr<Type>> _args;
    std::shared_ptr<Type> _return_type;
};

class ArrayType : public Type {
  public:
    explicit ArrayType(Type * t) : _elem_type(t) {}
    TypeVariant as_variant() override {
        return this;
    };
    ArrayType * shallow_copy() const override {
        return new ArrayType(*this);
    }

    Type * get_element_type() const {
        return _elem_type.get();
    }
  private:
    std::shared_ptr<Type> _elem_type;
};