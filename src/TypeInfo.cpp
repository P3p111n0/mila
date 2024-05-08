#include "TypeInfo.hpp"

namespace {
class BaseTypeResolver {
  public:
    explicit BaseTypeResolver(BaseType::Builtin t) : _t(t) {}

    bool operator()(const BaseType * ptr) { return ptr->id() == _t; }

    bool operator()(const Type *) { return false; }

  private:
    BaseType::Builtin _t;
};

class TypeEqualityVisitor {
  public:
    bool operator()(BaseType * lhs, BaseType * rhs) {
        return lhs->id() == rhs->id();
    }

    bool operator()(RefType * lhs, RefType * rhs) {
        auto lhs_reffd_type = lhs->get_referenced_type();
        auto rhs_reffd_type = rhs->get_referenced_type();

        return std::visit(*this, lhs_reffd_type->as_variant(),
                          rhs_reffd_type->as_variant());
    }

    bool operator()(FnType * lhs, FnType * rhs) {
        auto lhs_args = lhs->get_args();
        auto rhs_args = rhs->get_args();

        auto lhs_it = lhs_args.begin();
        auto rhs_it = rhs_args.begin();

        while (lhs_it != lhs_args.end() && rhs_it != rhs_args.end()) {
            if (!std::visit(*this, (*lhs_it)->as_variant(),
                            (*rhs_it)->as_variant())) {
                return false;
            }
            ++lhs_it, ++rhs_it;
        }

        if (lhs_it != lhs_args.end() || rhs_it != rhs_args.end()) {
            return false;
        }

        return std::visit(*this, lhs->get_return_type()->as_variant(),
                          rhs->get_return_type()->as_variant());
    }

    bool operator()(ArrayType * lhs, ArrayType * rhs) {
        // TODO array size
        return std::visit(*this, lhs->get_element_type()->as_variant(),
                          rhs->get_element_type()->as_variant());
    }

    bool operator()(Type * lhs, Type * rhs) { return false; }
};
} // namespace

bool TypeInfo::is_int(Type * ptr) {
    BaseTypeResolver btr(BaseType::Builtin::Int);
    return std::visit(btr, ptr->as_variant());
}

bool TypeInfo::is_double(Type * ptr) {
    BaseTypeResolver btr(BaseType::Builtin::Double);
    return std::visit(btr, ptr->as_variant());
}

bool TypeInfo::is_string(Type * ptr) {
    BaseTypeResolver btr(BaseType::Builtin::String);
    return std::visit(btr, ptr->as_variant());
}

bool TypeInfo::is_void(Type * ptr) {
    BaseTypeResolver btr(BaseType::Builtin::Void);
    return std::visit(btr, ptr->as_variant());
}

BaseType * TypeInfo::to_base_type(Type * ptr) {
    return dynamic_cast<BaseType *>(ptr);
}

bool TypeInfo::equal(Type * lhs, Type * rhs) {
    TypeEqualityVisitor x;
    return std::visit(x, lhs->as_variant(), rhs->as_variant());
}