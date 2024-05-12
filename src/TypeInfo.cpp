#include "TypeInfo.hpp"
#include "BaseTypeFactory.hpp"
#include <cassert>

namespace {
class BaseTypeResolver {
  public:
    explicit BaseTypeResolver(BaseType::Builtin t) : _t(t) {}

    bool operator()(const BaseType * ptr) { return ptr->id == _t; }

    bool operator()(const Type *) { return false; }

  private:
    BaseType::Builtin _t;
};

template <typename T> class IsTypeResolver {
  public:
    bool operator()(const T *) { return true; }

    bool operator()(const Type *) { return false; }
};

class TypeEqualityVisitor {
  public:
    bool operator()(BaseType * lhs, BaseType * rhs) {
        return lhs->id == rhs->id;
    }

    bool operator()(RefType * lhs, RefType * rhs) {
        auto lhs_reffd_type = lhs->base;
        auto rhs_reffd_type = rhs->base;

        return std::visit(*this, lhs_reffd_type->as_variant(),
                          rhs_reffd_type->as_variant());
    }

    bool operator()(FnType * lhs, FnType * rhs) {
        auto lhs_args = lhs->args;
        auto rhs_args = rhs->args;

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

        return std::visit(*this, lhs->return_type->as_variant(),
                          rhs->return_type->as_variant());
    }

    bool operator()(ArrayType * lhs, ArrayType * rhs) {
        if (lhs->lower_bound != rhs->lower_bound || lhs->upper_bound != rhs->upper_bound) {
            return false;
        }
        return std::visit(*this, lhs->elem_type->as_variant(),
                          rhs->elem_type->as_variant());
    }

    bool operator()(Type * lhs, MimicType * rhs) {
        for (auto & t : rhs->mimed_types) {
            if (std::visit(*this, lhs->as_variant(), t->as_variant())) {
                return true;
            }
        }
        return false;
    }

    /*
    bool operator()(MimicType * lhs, Type * rhs) {
        // swap args
        return std::visit(*this, rhs->as_variant(), lhs->as_variant());
    }
    */

    bool operator()(Type *, Type *) { return false; }
};

class TypeIdVisitor {
  public:
    TypeIdVisitor(bool use_special_characters)
        : _special_charactes(use_special_characters) {}
    std::string operator()(BaseType * t) {
        switch (t->id) {
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
        std::string reffd_name = std::visit(*this, ref->base->as_variant());
        if (_special_charactes) {
            reffd_name += "&";
        } else {
            reffd_name += "__ref";
        }
        return reffd_name;
    }

    std::string operator()(ArrayType * arr) {
        std::string elem_name = std::visit(*this, arr->elem_type->as_variant());
        if (_special_charactes) {
            elem_name += "[]";
        } else {
            elem_name += "__arr";
        }
        return elem_name;
    }

    std::string operator()(FnType * fn) {
        std::string ret_name = std::visit(*this, fn->return_type->as_variant());
        ret_name += " (";

        std::vector<std::shared_ptr<Type>> args = fn->args;
        for (size_t i = 0; i < args.size(); i++) {
            std::string arg_name = std::visit(*this, args[i]->as_variant());
            ret_name += arg_name;
            if (i != args.size() - 1) {
                ret_name += ", ";
            }
        }

        return ret_name;
    }

    std::string operator()(MimicType * mt) {
        std::string res = "glob";
        for (auto & arg : mt->mimed_types) {
            res += "_" + std::visit(*this, arg->as_variant());
        }
        return res;
    }

  private:
    bool _special_charactes;
};
} // namespace

bool type_info::is_int(type_ptr ptr) {
    BaseTypeResolver btr(BaseType::Builtin::Int);
    return std::visit(btr, ptr->as_variant());
}

bool type_info::is_double(type_ptr ptr) {
    BaseTypeResolver btr(BaseType::Builtin::Double);
    return std::visit(btr, ptr->as_variant());
}

bool type_info::is_string(type_ptr ptr) {
    BaseTypeResolver btr(BaseType::Builtin::String);
    return std::visit(btr, ptr->as_variant());
}

bool type_info::is_void(type_ptr ptr) {
    BaseTypeResolver btr(BaseType::Builtin::Void);
    return std::visit(btr, ptr->as_variant());
}

BaseType * type_info::to_base_type(Type * ptr) {
    return dynamic_cast<BaseType *>(ptr);
}

std::shared_ptr<BaseType> type_info::to_base_type(type_ptr ptr) {
    return std::dynamic_pointer_cast<BaseType>(ptr);
}

RefType * type_info::to_ref_type(Type * ptr) {
    return dynamic_cast<RefType *>(ptr);
}

std::shared_ptr<RefType> type_info::to_ref_type(type_ptr ptr) {
    return std::dynamic_pointer_cast<RefType>(ptr);
}

ArrayType * type_info::to_array_type(Type * ptr) {
    return dynamic_cast<ArrayType *>(ptr);
}

std::shared_ptr<ArrayType> type_info::to_array_type(type_ptr ptr) {
    return dynamic_pointer_cast<ArrayType>(ptr);
}

bool type_info::equal(type_ptr lhs, type_ptr rhs) {
    TypeEqualityVisitor x;
    return std::visit(x, lhs->as_variant(), rhs->as_variant()) ||
           std::visit(x, rhs->as_variant(), lhs->as_variant());
}

type_ptr type_info::get_common_type(type_ptr lhs, type_ptr rhs) {
    BaseTypeFactory btf;

    if (is_mimic_type(lhs) && equal(lhs, rhs)) {
        return rhs;
    } else if (is_mimic_type(rhs) && equal(lhs, rhs)) {
        return lhs;
    }

    // type equality
    if (equal(lhs, rhs)) {
        return lhs;
    }

    // promotion to double
    if ((is_int(lhs) && is_double(rhs)) || (is_double(lhs) && is_int(rhs))) {
        return type_ptr(btf.get_double_t());
    }

    // no conversion possible
    return nullptr;
}

std::string type_info::get_type_identifier(type_ptr ptr) {
    TypeIdVisitor x(true);
    return std::visit(x, ptr->as_variant());
}

std::string type_info::get_printable_id(type_ptr ptr) {
    TypeIdVisitor x(false);
    return std::visit(x, ptr->as_variant());
}

bool type_info::is_convertible(type_ptr src, type_ptr dst) {
    if (equal(src, dst)) {
        return true;
    }

    if ((is_int(src) && is_double(dst)) || (is_double(src) && is_int(dst))) {
        return true;
    }

    return false;
}

bool type_info::is_ref_type(type_ptr ptr) {
    IsTypeResolver<RefType> x;
    return std::visit(x, ptr->as_variant());
}

bool type_info::is_base_type(type_ptr ptr) {
    IsTypeResolver<BaseType> x;
    return std::visit(x, ptr->as_variant());
}

bool type_info::is_mimic_type(type_ptr ptr) {
    IsTypeResolver<MimicType> x;
    return std::visit(x, ptr->as_variant());
}

bool type_info::is_array_type(type_ptr ptr) {
    IsTypeResolver<ArrayType> x;
    return std::visit(x, ptr->as_variant());
}