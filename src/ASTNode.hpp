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

class ASTNodeAdd : public ASTNodeBinary {
  public:
    ASTNodeAdd(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeSub : public ASTNodeBinary {
  public:
    ASTNodeSub(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeOr : public ASTNodeBinary {
  public:
    ASTNodeOr(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeXor : public ASTNodeBinary {
  public:
    ASTNodeXor(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeEq : public ASTNodeBinary {
  public:
    ASTNodeEq(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeNotEq : public ASTNodeBinary {
  public:
    ASTNodeNotEq(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeLt : public ASTNodeBinary {
  public:
    ASTNodeLt(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeGt : public ASTNodeBinary {
  public:
    ASTNodeGt(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeLtE : public ASTNodeBinary {
  public:
    ASTNodeLtE(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeGtE : public ASTNodeBinary {
  public:
    ASTNodeGtE(ASTNode * lhs, ASTNode * rhs) : ASTNodeBinary(lhs, rhs) {}
};

class ASTNodeBody : public ASTNode {
  public:
    ASTNodeBody(std::list<std::shared_ptr<ASTNode>> statements)
        : _stmts(std::move(statements)) {}

  private:
    std::list<std::shared_ptr<ASTNode>> _stmts;
};

class ASTNodeAssign : public ASTNode {
  public:
    ASTNodeAssign(ASTNode * lhs, ASTNode * rhs) : _lhs(lhs), _rhs(rhs) {}

  private:
    std::shared_ptr<ASTNode> _lhs;
    std::shared_ptr<ASTNode> _rhs;
};

class ASTNodeExit : public ASTNode {
  public:
    ASTNodeExit() = default;
};

class ASTNodeIf : public ASTNode {
  public:
    ASTNodeIf(ASTNode * cond, ASTNode * body)
        : _cond(cond), _body(body), _else(std::nullopt) {}
    ASTNodeIf(ASTNode * cond, ASTNode * body, ASTNode * else_b)
        : _cond(cond), _body(body), _else(std::shared_ptr<ASTNode>(else_b)) {}

  private:
    std::shared_ptr<ASTNode> _cond;
    std::shared_ptr<ASTNode> _body;
    std::optional<std::shared_ptr<ASTNode>> _else;
};