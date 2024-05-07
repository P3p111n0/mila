#include "TypeInfo.hpp"

namespace {
    class BaseTypeResolver {
      public:
        explicit BaseTypeResolver(BaseType::Builtin t) : _t(t) {}

        bool operator()(const BaseType * ptr) {
            return ptr->id() == _t;
        }

        bool operator()(const Type *) {
            return false;
        }

      private:
        BaseType::Builtin _t;
    };
}

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