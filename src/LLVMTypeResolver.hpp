#pragma once

#include "Type.hpp"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

class LLVMTypeResolver {
  public:
    explicit LLVMTypeResolver(llvm::LLVMContext & ctx) : _ctx(ctx) {}
    llvm::Type * operator()(BaseType *);
    llvm::Type * operator()(ArrayType *);
    llvm::Type * operator()(RefType *);
    llvm::Type * operator()(MimicType *);
    llvm::Type * operator()(Type *);

  private:
    llvm::LLVMContext & _ctx;
};

