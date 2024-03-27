#pragma once

#include <iostream>
#include <string>
#include <variant>

enum class TokenType {
    EOI,
    Semicolon,
    Colon,
    Comma,

    // keywords
    Program,
    Const,
    Var,
    Integer,
    Begin,
    End,
    If,
    Then,
    Else,
    While,
    Do,
    For,
    Function,
    Procedure,
    Exit,
    To,
    Downto,

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
    Op_Xor,
    Op_Not,

    // values
    IntVal,
    Identifier
};

struct Position {
    Position() : row(0), column(0) {}
    void advance() {
        column++;
    }
    void new_line() {
        row++;
        column = 0;
    }
    friend std::ostream & operator<<(std::ostream & os, const Position & pos) {
        return os << pos.row << ':' << pos.column;
    }
    std::size_t row;
    std::size_t column;
};

class Token {
  public:
    Token(TokenType type, int val, Position pos = {}) : pos(pos), _type(type), _val(val) {}
    Token(TokenType type, std::string val, Position pos = {}) : pos(pos), _val(std::move(val)) {}
    TokenType type() const { return _type; }
    int get_int() const {
        switch (_val.index()) {
        case 0:
            return std::get<int>(_val);
        case 1:
            return 0;
        }
    }
    std::string get_str() const {
        switch (_val.index()) {
        case 0:
            return std::to_string(std::get<int>(_val));
        case 1:
            return std::get<std::string>(_val);
        }
    }

    Position pos;
  private:
    TokenType _type;
    std::variant<int, std::string> _val;
};

class Lexer {
  public:
    Lexer() : _current(TokenType::EOI, 0), _in(std::cin) {
        _current = next_token();
    }
    Lexer(std::istream & in) : _current(TokenType::EOI, 0), _in(in) {
        _current = next_token();
    }
    ~Lexer() = default;

    const Token & peek() const { return _current; }

    Token get() {
        Token old = _current;
        _current = next_token();
        return old;
    }

    bool match(TokenType);

  private:
    Token next_token();

    Token _current;
    std::istream & _in;
    Position _pos;
};