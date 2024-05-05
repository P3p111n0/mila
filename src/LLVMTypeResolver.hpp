#pragma once

#include "Type.hpp"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

class LLVMTypeResolver {
  public:
    explicit LLVMTypeResolver(llvm::LLVMContext & ctx) : _ctx(ctx) {}
    llvm::Type * operator()(BaseType *);
    llvm::Type * operator()(RefType *);
    llvm::Type * operator()(FnType *);
    llvm::Type * operator()(ArrayType *);

  private:
    llvm::LLVMContext & _ctx;
};

