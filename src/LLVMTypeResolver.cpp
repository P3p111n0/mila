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