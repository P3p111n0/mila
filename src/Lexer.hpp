#pragma once

#include <string>
#include <iostream>
#include <variant>

enum class TokenType {
    EOI,

    // keywords
    Program,
    Const,
    Var,
    Integer,
    Begin,
    End,
    If,
    Then,
    While,
    Do,
    For,
    Function,
    Procedure,
    Exit,

    // symbols
    Par_Open,
    Par_Close,

    // operators
    Op_Plus,
    Op_Minus,
    Op_Mul,
    Op_Div,
    Op_Mod,
    Op_Equal,
    Op_NotEqual,
    Op_Assign,
    Op_Gt,
    Op_Lt,
    Op_LtE,
    Op_GtE,
    Op_And,
    Op_Or,
    Op_Not,

    // values
    IntVal,
    Identifier
};

class Token {
public:
    Token(TokenType type, int val) : _type(type), _val(val) {}
    Token(TokenType type, std::string val) : _val(std::move(val)) {}
    TokenType type() const {
        return _type;
    }
    int get_int() const {
        switch(_val.index()) {
            case 0:
                return std::get<int>(_val);
            case 1:
                return 0;
        }
    }
    std::string get_str() const {
        switch(_val.index()) {
            case 0:
                return std::to_string(std::get<int>(_val));
            case 1:
                return std::get<std::string>(_val);
        }
    }
private:
    TokenType _type;
    std::variant<int, std::string> _val;
};

class Lexer {
public:
    Lexer(std::istream & in) : _current(TokenType::EOI, 0), _in(in) {
        _current = next_token();
    }
    ~Lexer() = default;

    int get_int() const;
    const std::string get_str() const;

    const Token & peek() const {
        return _current;
    }

    Token get() {
        Token old = _current;
        _current = next_token();
        return old;
    }
private:
    Token next_token();

    Token _current;
    std::istream & _in;
};