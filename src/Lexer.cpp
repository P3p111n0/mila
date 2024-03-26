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
    {"else", Token(TokenType::Else, "else")}};

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
        goto q0;
    }
    if (isdigit(c)) {
        goto dec;
    }

    switch (c) {
    case '(':
        return Token(TokenType::Par_Open, "(");
    case ')':
        return Token(TokenType::Par_Close, ")");
    case ';':
        return Token(TokenType::Semicolon, ";");
    case ',':
        return Token(TokenType::Comma, ",");
    case '0': {
        str_val += c;
        goto zero;
    }
    case '$':
        goto hex;
    case '&': {
        goto octal;
    }
    case '+':
        str_val += c;
        goto plus;
    case '-':
        str_val += c;
        goto minus;
    case ':':
        str_val += c;
        goto colon;
    case '*':
    case '/':
    case '<':
    case '>':
    case '=': {
        str_val += c;
        goto op;
    }
    default: {
        str_val += c;
        goto identifier;
    }
    }

plus:
    c = _in.peek();
    if (isdigit(c)) {
        goto dec;
    }
    return _op_map.at("+");

minus:
    c = _in.peek();
    if (isdigit(c)) {
        goto dec;
    }
    return _op_map.at("-");

zero:
    c = _in.get();
    switch (c) {
    case 'x':
    case 'X': {
        str_val += c;
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
        goto octal;
    }
    default: {
        int_val = 0;
        return Token(TokenType::IntVal, 0);
    }
    }

hex:
    c = _in.peek();
    if (is_hex_digit(c)) {
        (void)_in.get();
        str_val += c;
        goto hex;
    }
    int_val = std::stoi(str_val, nullptr, 16);
    return Token(TokenType::IntVal, int_val);

octal:
    c = _in.peek();
    if (is_octal_digit(c)) {
        (void)_in.get();
        str_val += c;
        goto octal;
    }
    int_val = std::stoi(str_val, nullptr, 8);
    return Token(TokenType::IntVal, int_val);

dec:
    c = _in.peek();
    if (isdigit(c)) {
        (void)_in.get();
        str_val += c;
        goto dec;
    }
    int_val = std::stoi(str_val, nullptr, 10);
    return Token(TokenType::IntVal, int_val);

colon:
    c = _in.peek();
    if (c == '=') {
        goto op;
    }
    return Token(TokenType::Colon, ":");

op:
    c = _in.peek();
    switch (c) {
    case '=':
    case '>': {
        (void)_in.get();
        str_val += c;
    }
    }
    return _op_map.at(str_val);

identifier:
    c = _in.peek();
    if (!isspace(c) && is_id_symbol(c)) {
        (void)_in.get();
        str_val += c;
        goto identifier;
    }

    if (_keyword_map.contains(str_val)) {
        return _keyword_map.at(str_val);
    }
    return Token(TokenType::Identifier, str_val);
}