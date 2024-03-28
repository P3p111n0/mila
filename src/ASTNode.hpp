#pragma once

#include <list>
#include <memory>
#include <optional>

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
  public:
    enum class Operator {
        Not
    };

    ASTNodeUnary(ASTNode * arg, Operator op) : _arg(arg), _op(op) {}
  private:
    std::shared_ptr<ASTNode> _arg;
    Operator _op;
};

class ASTNodeBinary : public ASTNode {
  public:
    enum class Operator {
        Add,
        Sub,
        Mul,
        Div,
        Or,
        Xor,
        And,
        Mod,
        Eq,
        NEq,
        Lt,
        Gt,
        LtE,
        GtE
    };


    ASTNodeBinary(ASTNode * lhs, ASTNode * rhs, Operator op) : _lhs(lhs), _rhs(rhs), _op(op) {}
  private:
    std::shared_ptr<ASTNode> _lhs;
    std::shared_ptr<ASTNode> _rhs;
    Operator _op;
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
    ASTNodeAssign(std::string target, ASTNode * rhs) : _target(std::move(target)), _rhs(rhs) {}

  private:
    std::string _target;
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

class ASTNodeWhile : public ASTNode {
  public:
    ASTNodeWhile(ASTNode * cond, ASTNode * body) : _cond(cond), _body(body) {}

  private:
    std::shared_ptr<ASTNode> _cond;
    std::shared_ptr<ASTNode> _body;
};

class ASTNodeFor : public ASTNode {
  public:
    ASTNodeFor(std::string var, ASTNode * start, ASTNode * stop, ASTNode * body, bool is_downto)
        : _var(std::move(var)), _it_start(start), _it_stop(stop), _body(body), _is_downto(is_downto) {}

  private:
    std::string _var;
    std::shared_ptr<ASTNode> _it_start;
    std::shared_ptr<ASTNode> _it_stop;
    std::shared_ptr<ASTNode> _body;
    bool _is_downto;
};

class ASTNodeBreak : public ASTNode {
  public:
    ASTNodeBreak() = default;
};

class ASTNodeCall : public ASTNode {
  public:
    ASTNodeCall(std::string fun, std::list<std::shared_ptr<ASTNode>> args)
        : _fn(std::move(fun)), _args(std::move(args)) {}

  private:
    std::string _fn;
    std::list<std::shared_ptr<ASTNode>> _args;
};

class ASTNodeMain : public ASTNode {
  public:
    ASTNodeMain(std::string name, std::list<std::shared_ptr<ASTNode>> main_body)
        : _pg_name(std::move(name)), _main_body(std::move(main_body)) {}

  private:
    std::string _pg_name;
    std::list<std::shared_ptr<ASTNode>> _main_body;
};