#include "LLVMTypeResolver.hpp"

llvm::Type * LLVMTypeResolver::operator()(FnType *) {
    // this shouldn't happen
    return nullptr;
}

llvm::Type * LLVMTypeResolver::operator()(RefType *) {
    // this shouldn't happen
    return nullptr;
}

llvm::Type * LLVMTypeResolver::operator()(ArrayType *) {
    // this shouldn't happen
    return nullptr;
}

llvm::Type * LLVMTypeResolver::operator()(BaseType * t) {
    auto id = t->id();
    if (id == "int") {
        return llvm::Type::getInt32Ty(_ctx);
    } else if (id == "double") {
        return llvm::Type::getDoubleTy(_ctx);
    } else if (id == "void") {
        return llvm::Type::getVoidTy(_ctx);
    } else if (id == "string") {
        return llvm::Type::getInt8PtrTy(_ctx);
    } else {
        //this shouldn't happen
        return nullptr;
    }
}