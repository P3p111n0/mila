#pragma once

#include "ASTNode.hpp"
#include "SymbolTable.hpp"
#include <memory>
#include <stack>
#include <string>

class LambdaLifter {
  public:
    LambdaLifter(std::shared_ptr<SymbolTable> st) : _st(std::move(st)) {}
    void lift_tree(std::shared_ptr<ASTNode>);

    void operator()(ASTNode *);
    void operator()(ASTNodeBlock *);
    void operator()(ASTNodeBody *);
    void operator()(ASTNodeVar *);
    void operator()(ASTNodeVarByRef *);
    void operator()(ASTNodeFunction *);
    void operator()(ASTNodeCall *);
    void operator()(ASTNodeBinary *);
    void operator()(ASTNodeUnary *);
    void operator()(ASTNodeWhile *);
    void operator()(ASTNodeFor *);
    void operator()(ASTNodeAssign *);
    void operator()(ASTNodeIf *);
    void operator()(ASTNodeIdentifier *);
    void operator()(ASTNodeArrAccess *);


  private:
    void lift(std::shared_ptr<ASTNodeAssignable>);

    std::shared_ptr<SymbolTable> _st;
    std::stack<ASTNodeFunction*> _parent_fn;
};