#pragma once

#include <memory>
#include <vector>
#include <string>

class Type {
  public:
    Type() = default;
    virtual ~Type() = default;
};

class BaseType : public Type {
  public:
    explicit BaseType(std::string id) : _id(std::move(id)) {}
    const std::string & id() const { return _id; }

  private:
    std::string _id;
};

class RefType : public Type {
  public:
    explicit RefType(Type * t) : _base(t) {}
    const Type * get_referenced_type() const { return _base.get(); }

  private:
    std::shared_ptr<Type> _base;
};

class FnType : public Type {
  public:
    FnType(std::vector<std::shared_ptr<Type>> args, Type * rtype)
        : _args(std::move(args)), _return_type(rtype) {}

  private:
    std::vector<std::shared_ptr<Type>> _args;
    std::shared_ptr<Type> _return_type;
};

class ArrayType : public Type {
  public:
    explicit ArrayType(Type * t) : _elem_type(t) {}
  private:
    std::shared_ptr<Type> _elem_type;
};