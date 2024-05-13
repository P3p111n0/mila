#pragma once

#include "ASTNode.hpp"
#include "SymbolTable.hpp"
#include <optional>

template <typename T> class ExprEvaluator {
  public:
    explicit ExprEvaluator(std::shared_ptr<SymbolTable> symbol_table)
        : _st(std::move(symbol_table)) {}
    std::optional<T> eval(ASTNode * ptr) const {
        return std::visit(*this, ptr->as_variant());
    }
    std::optional<T> operator()(const ASTNode *) const {
        return std::nullopt;
    }
    std::optional<T> operator()(const ASTNodeInt * inode) const {
        return inode->val;
    }
    std::optional<T> operator()(const ASTNodeUnary * unary) const {
        std::optional<T> arg = std::visit(*this, unary->arg->as_variant());
        if (!arg.has_value()) {
            return std::nullopt;
        }
        T value = arg.value();
        switch(unary->op) {
        case ASTNodeUnary::Operator::Not: {
            value = ~value;
            break;
        }
        default:
            assert(0 && "unreachable");
        }
        return value;
    }
    std::optional<T> operator()(const ASTNodeBinary * op) const {
        std::optional<int> lhs_opt = std::visit(*this, op->lhs->as_variant());
        std::optional<int> rhs_opt = std::visit(*this, op->rhs->as_variant());
        if (!lhs_opt.has_value() || !rhs_opt.has_value()) {
            return std::nullopt;
        }
        T lhs = lhs_opt.value();
        T rhs = rhs_opt.value();
        T res = 0;
        switch(op->op) {
        case ASTNodeBinary::Operator::Add: {
            res = lhs + rhs;
            break;
        }
        case ASTNodeBinary::Operator::Sub: {
            res = lhs - rhs;
            break;
        }
        case ASTNodeBinary::Operator::Mul: {
            res = lhs * rhs;
            break;
        }
        case ASTNodeBinary::Operator::Div: {
            res = lhs / rhs;
            break;
        }
        case ASTNodeBinary::Operator::Or: {
            res = lhs | rhs;
            break;
        }
        case ASTNodeBinary::Operator::Xor: {
            res = lhs ^ rhs;
            break;
        }
        case ASTNodeBinary::Operator::And: {
            res = lhs & rhs;
            break;
        }
        case ASTNodeBinary::Operator::Mod: {
            res = lhs % rhs;
            break;
        }
        case ASTNodeBinary::Operator::Eq: {
            res = (lhs == rhs);
            break;
        }
        case ASTNodeBinary::Operator::NEq: {
            res = (lhs != rhs);
            break;
        }
        case ASTNodeBinary::Operator::Lt: {
            res = (lhs < rhs);
            break;
        }
        case ASTNodeBinary::Operator::Gt: {
            res = (lhs > rhs);
            break;
        }
        case ASTNodeBinary::Operator::LtE: {
            res = (lhs <= rhs);
            break;
        }
        case ASTNodeBinary::Operator::GtE: {
            res = (lhs >= rhs);
            break;
        }
        default:
            assert(0 && "unreachable");
        }
        return res;
    }
    std::optional<T> operator()(const ASTNodeFBinary *) const {
        assert(0 && "todo");
        return std::nullopt;
    }
    std::optional<T> operator()(const ASTNodeIdentifier * id) const {
        auto lookup = _st->lookup_constant(id->name);
        if (!lookup.has_value()) {
            return std::nullopt;
        }
        return std::visit(*this, lookup.value()->as_variant());
    }
  private:
    std::shared_ptr<SymbolTable> _st;
};