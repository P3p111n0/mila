#include "TypeInfo.hpp"
#include "BaseTypeFactory.hpp"
#include <cassert>

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

    bool operator()(Type *, Type *) { return false; }
};

class TypeIdVisitor {
  public:
    std::string operator()(BaseType * t) {
        switch(t->id()) {
        case BaseType::Builtin::Int:
            return "int";
        case BaseType::Builtin::Double:
            return "double";
        case BaseType::Builtin::String:
            return "string";
        case BaseType::Builtin::Void:
            return "void";
        default:
            assert(0 && "unreachable");
        }
    }

    std::string operator()(RefType * ref) {
        std::string reffd_name = std::visit(*this, ref->get_referenced_type()->as_variant());
        reffd_name += "&";
        return reffd_name;
    }

    std::string operator()(ArrayType * arr) {
        std::string elem_name = std::visit(*this, arr->get_element_type()->as_variant());
        elem_name += "[]";
        return elem_name;
    }

    std::string operator()(FnType * fn) {
        std::string ret_name = std::visit(*this, fn->get_return_type()->as_variant());
        ret_name += " (";

        std::vector<std::shared_ptr<Type>> args = fn->get_args();
        for (size_t i = 0; i < args.size(); i++) {
            std::string arg_name = std::visit(*this, args[i]->as_variant());
            ret_name += arg_name;
            if (i != args.size() - 1) {
                ret_name += ", ";
            }
        }

        return ret_name;
    }
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

Type * TypeInfo::get_common_type(Type * lhs, Type * rhs) {
    BaseTypeFactory btf;
    // type equality
    if (equal(lhs, rhs)) {
        return lhs;
    }

    // promotion to double
    if ((is_int(lhs) && is_double(rhs)) || (is_double(lhs) && is_int(rhs))) {
        return btf.get_double_t();
    }

    // no conversion possible
    return nullptr;
}

std::string TypeInfo::get_type_identifier(Type * ptr) {
    TypeIdVisitor x;
    return std::visit(x, ptr->as_variant());
}

bool TypeInfo::is_convertible(Type * src, Type * dst) {
    if (equal(src, dst)) {
        return true;
    }

    if ((is_int(src) && is_double(dst)) || (is_double(src) && is_int(dst))) {
        return true;
    }

    return false;
}