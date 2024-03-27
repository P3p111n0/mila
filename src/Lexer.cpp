#include "Lexer.hpp"
#include <string>
#include <unordered_map>

static inline bool is_hex_digit(char c) {
    return (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f') || isdigit(c);
}

static inline bool is_octal_digit(char c) { return c >= '0' && c <= '7'; }

static inline bool is_id_symbol(char c) { return isalnum(c) || c == '_'; }

const std::unordered_map<std::string, Token> _keyword_map = {
    {"program", Token(TokenType::Program, "program")},
    {"const", Token(TokenType::Const, "const")},
    {"var", Token(TokenType::Var, "var")},
    {"integer", Token(TokenType::Integer, "integer")},
    {"begin", Token(TokenType::Begin, "begin")},
    {"end", Token(TokenType::End, "end")},
    {"if", Token(TokenType::If, "if")},
    {"then", Token(TokenType::Then, "then")},
    {"while", Token(TokenType::While, "while")},
    {"do", Token(TokenType::Do, "do")},
    {"for", Token(TokenType::For, "for")},
    {"function", Token(TokenType::Function, "function")},
    {"procedure", Token(TokenType::Procedure, "procedure")},
    {"exit", Token(TokenType::Exit, "exit")},
    {"and", Token(TokenType::Op_And, "and")},
    {"or", Token(TokenType::Op_Or, "or")},
    {"div", Token(TokenType::Op_Div, "div")},
    {"mod", Token(TokenType::Op_Mod, "mod")},
    {"not", Token(TokenType::Op_Not, "not")},
    {"xor", Token(TokenType::Op_Xor, "xor")},
    {"to", Token(TokenType::To, "to")},
    {"downto", Token(TokenType::Downto, "downto")},
    {"else", Token(TokenType::Else, "else")},
    {"break", Token(TokenType::Break, "else")}};

const std::unordered_map<std::string, Token> _op_map = {
    {"+", Token(TokenType::Op_Plus, "+")},
    {"-", Token(TokenType::Op_Minus, "-")},
    {"*", Token(TokenType::Op_Mul, "*")},
    {"/", Token(TokenType::Op_Div, "/")},
    {"<", Token(TokenType::Op_Lt, "<")},
    {"<=", Token(TokenType::Op_LtE, "<=")},
    {">", Token(TokenType::Op_Gt, ">")},
    {">=", Token(TokenType::Op_GtE, ">=")},
    {"<>", Token(TokenType::Op_NotEqual, "<>")},
    {"=", Token(TokenType::Op_Equal, "=")},
    {":=", Token(TokenType::Op_Assign, ":=")},
};

bool Lexer::match(TokenType t) {
    if (t != _current.type()) {
        _current = next_token();
        return false;
    }
    _current = next_token();
    return true;
}

Token Lexer::next_token() {
    char c;
    int int_val = 0;
    std::string str_val;

q0:
    c = _in.get();
    if (_in.eof()) {
        return Token(TokenType::EOI, 0);
    }
    if (isspace(c)) {
        _pos.advance(c);
        goto q0;
    }
    if (isdigit(c)) {
        goto dec;
    }

    switch (c) {
    case '(':
        return Token(TokenType::Par_Open, "(", _pos);
    case ')':
        return Token(TokenType::Par_Close, ")", _pos);
    case ';':
        return Token(TokenType::Semicolon, ";", _pos);
    case ',':
        return Token(TokenType::Comma, ",", _pos);
    case '.':
        return Token(TokenType::Period, ".", _pos);
    case '{':
        goto comment;
    case '0': {
        str_val += c;
        _pos.advance(c);
        goto zero;
    }
    case '$':
        goto hex;
    case '&': {
        goto octal;
    }
    case '+':
        str_val += c;
        _pos.advance(c);
        goto plus;
    case '-':
        str_val += c;
        _pos.advance(c);
        goto minus;
    case ':':
        str_val += c;
        _pos.advance(c);
        goto colon;
    case '*':
    case '/':
    case '<':
    case '>':
    case '=': {
        str_val += c;
        _pos.advance(c);
        goto op;
    }
    default: {
        str_val += c;
        _pos.advance(c);
        goto identifier;
    }
    }

plus: {
    c = _in.peek();
    if (isdigit(c)) {
        goto dec;
    }
    auto tok = _op_map.at("+");
    tok.pos = _pos;
    return tok;
}

minus: {
    c = _in.peek();
    if (isdigit(c)) {
        goto dec;
    }
    auto tok = _op_map.at("-");
    tok.pos = _pos;
    return tok;
}

zero: {
    c = _in.get();
    switch (c) {
    case 'x':
    case 'X': {
        str_val += c;
        _pos.advance(c);
        goto hex;
    }
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7': {
        str_val += c;
        _pos.advance(c);
        goto octal;
    }
    default: {
        int_val = 0;
        return Token(TokenType::IntVal, 0, _pos);
    }
    }
}

hex: {
    c = _in.peek();
    if (is_hex_digit(c)) {
        (void)_in.get();
        str_val += c;
        _pos.advance(c);
        goto hex;
    }
    int_val = std::stoi(str_val, nullptr, 16);
    return Token(TokenType::IntVal, int_val, _pos);
}

octal: {
    c = _in.peek();
    if (is_octal_digit(c)) {
        (void)_in.get();
        str_val += c;
        _pos.advance(c);
        goto octal;
    }
    int_val = std::stoi(str_val, nullptr, 8);
    return Token(TokenType::IntVal, int_val, _pos);
}

dec: {
    c = _in.peek();
    if (isdigit(c)) {
        (void)_in.get();
        str_val += c;
        _pos.advance(c);
        goto dec;
    }
    int_val = std::stoi(str_val, nullptr, 10);
    return Token(TokenType::IntVal, int_val, _pos);
}

colon: {
    c = _in.peek();
    if (c == '=') {
        goto op;
    }
    return Token(TokenType::Colon, ":", _pos);
}

op: {
    c = _in.peek();
    switch (c) {
    case '=':
    case '>': {
        (void)_in.get();
        str_val += c;
        _pos.advance(c);
    }
    }
    auto tok = _op_map.at(str_val);
    tok.pos = _pos;
    return tok;
}

identifier: {
    c = _in.peek();
    if (!isspace(c) && is_id_symbol(c)) {
        (void)_in.get();
        str_val += c;
        _pos.advance(c);
        goto identifier;
    }

    if (_keyword_map.contains(str_val)) {
        auto tok = _keyword_map.at(str_val);
        tok.pos = _pos;
        return tok;
    }
    return Token(TokenType::Identifier, str_val, _pos);
}

comment: {
    c = _in.get();
    if (c != '}') {
        _pos.advance(c);
        goto comment;
    }
    goto q0;
};

}