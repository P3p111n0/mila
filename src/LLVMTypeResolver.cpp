#include "LLVMTypeResolver.hpp"

llvm::Type * LLVMTypeResolver::operator()(Type *) {
    assert(0 && "this shouldn't happen");
}

llvm::Type * LLVMTypeResolver::operator()(BaseType * t) {
    auto id = t->id;
    switch (id) {
    case BaseType::Builtin::Int:
        return llvm::Type::getInt32Ty(_ctx);
    case BaseType::Builtin::Double:
        return llvm::Type::getDoubleTy(_ctx);
    case BaseType::Builtin::Void:
        return llvm::Type::getVoidTy(_ctx);
    case BaseType::Builtin::String:
        return llvm::Type::getInt8PtrTy(_ctx);
    default:
        //this shouldn't happen
        return nullptr;
    }
}

llvm::Type * LLVMTypeResolver::operator()(ArrayType * ptr) {
    llvm::Type * elem = std::visit(*this, ptr->elem_type->as_variant());
    llvm::Type * type = llvm::ArrayType::get(elem, ptr->size());
    return type;
}

llvm::Type * LLVMTypeResolver::operator()(RefType * ptr) {
    llvm::Type * underlying_type = std::visit(*this, ptr->base->as_variant());
    return underlying_type->getPointerTo();
}

llvm::Type * LLVMTypeResolver::operator()(MimicType *) {
    assert(0 && "mimic is intended only as a compilation intermediate.");
}