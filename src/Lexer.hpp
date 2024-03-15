#pragma once

#include <string>
#include <iostream>

enum class Token {
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

class Lexer {
public:
    Lexer() = default;
    ~Lexer() = default;

    int get_int() const;
    const std::string get_str() const;

    Token next_token(std::istream &);
private:
    int _int_val;
    std::string _str_val;
};