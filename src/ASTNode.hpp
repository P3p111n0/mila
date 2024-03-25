#pragma once

#include <memory>

class ASTNode {
  public:
    ASTNode() = default;
    virtual ~ASTNode() = default;
};

class ASTNodeInt : public ASTNode {
  public:
    ASTNodeInt(int val) : _val(val) {}
  private:
    int _val;
};

class ASTNodeIdentifier : public ASTNode {
  public:
    ASTNodeIdentifier(std::string name) : _name(std::move(name)) {}
  private:
    std::string _name;
};

class ASTNodeUnary : public ASTNode {
  protected:
    ASTNodeUnary(ASTNode * arg) : _arg(arg) {}
    std::shared_ptr<ASTNode> _arg;
};

class ASTNodeUnaryMinus : public ASTNodeUnary {
  public:
    ASTNodeUnaryMinus(ASTNode * arg) : ASTNodeUnary(arg) {}
};

class ASTNodeNot : public ASTNodeUnary {
  public:
    ASTNodeNot(ASTNode * arg) : ASTNodeUnary(arg) {}
};

class ASTNodeBinary : public ASTNode {
  protected:
    ASTNodeBinary(ASTNode * lhs, ASTNode * rhs) : _lhs(lhs), _rhs(rhs) {}
    std::shared_ptr<ASTNode> _lhs;
    std::shared_ptr<ASTNode> _rhs;
};

class ASTNodeMul : public ASTNodeBinary {
  public:
    ASTNodeMul(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeDiv : public ASTNodeBinary {
  public:
    ASTNodeDiv(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeMod : public ASTNodeBinary {
  public:
    ASTNodeMod(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeAnd : public ASTNodeBinary {
  public:
    ASTNodeAnd(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};