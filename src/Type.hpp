#pragma once

#include <memory>
#include <vector>
#include <string>
#include <variant>

class BaseType;
class RefType;
class FnType;
class ArrayType;

using TypeVariant = std::variant<BaseType*, RefType*, FnType*, ArrayType*>;

class Type {
  public:
    Type() = default;
    virtual ~Type() = default;
    virtual TypeVariant as_variant() = 0;
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

  private:
    Builtin _id;
};

class RefType : public Type {
  public:
    explicit RefType(Type * t) : _base(t) {}
    const Type * get_referenced_type() const { return _base.get(); }
    TypeVariant as_variant() override {
        return this;
    };

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
  private:
    std::shared_ptr<Type> _elem_type;
};