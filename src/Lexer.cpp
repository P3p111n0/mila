#include "Lexer.hpp"
#include <unordered_map>
#include <string>

static inline bool is_hex_digit(char c) {
    return (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f') || isdigit(c);
}

static inline bool is_octal_digit(char c) {
    return c >= '0' && c <= '7';
}

static inline bool is_id_symbol(char c) {
    return isalnum(c) || c == '_';
}

const std::unordered_map<std::string, Token> _keyword_map = {
        {"program", Token::Program},
        {"const", Token::Const},
        {"var", Token::Var},
        {"integer", Token::Integer},
        {"begin", Token::Begin},
        {"end", Token::End},
        {"if", Token::If},
        {"then", Token::Then},
        {"while", Token::While},
        {"do", Token::Do},
        {"for", Token::For},
        {"function", Token::Function},
        {"procedure", Token::Procedure},
        {"exit", Token::Exit},
        {"and", Token::Op_And},
        {"or", Token::Op_Or},
        {"div", Token::Op_Div},
        {"mod", Token::Op_Mod},
        {"not", Token::Op_Not}
};

const std::unordered_map<std::string, Token> _op_map = {
        {"+", Token::Op_Plus},
        {"-", Token::Op_Minus},
        {"*", Token::Op_Mul},
        {"/", Token::Op_Div},
        {"<", Token::Op_Lt},
        {"<=", Token::Op_LtE},
        {">", Token::Op_Gt},
        {">=", Token::Op_GtE},
        {"<>", Token::Op_NotEqual},
        {"=", Token::Op_Equal},
        {":=", Token::Op_Assign},
};

int Lexer::get_int() const {
    return _int_val;
}

const std::string Lexer::get_str() const {
    return _str_val;
}

Token Lexer::next_token(std::istream & in) {
    char c;
    _int_val = 0;
    _str_val = {};

    q0:
        c = in.get();
        if (in.eof()) {
            return Token::EOI;
        }
        if (isspace(c)) {
            goto q0;
        }
        if (isdigit(c)) {
            goto dec;
        }

        switch(c) {
            case '(':
                return Token::Par_Open;
            case ')':
                return Token::Par_Close;
            case '0': {
                _str_val += c;
                goto zero;
            }
            case '$':
                goto hex;
            case '&': {
                goto octal;
            }
            case '+':
            case '-':
            case '*':
            case '/':
            case '%':
            case '<':
            case '>':
            case ':':
            case '=': {
                _str_val += c;
                goto op;
            }
            default: {
                _str_val += c;
                goto identifier;
            }
        }

    zero:
        c = in.get();
        switch(c) {
            case 'x':
            case 'X': {
                _str_val += c;
                goto hex;
            }
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7': {
                _str_val += c;
                goto octal;
            }
            default: {
                _int_val = 0;
                return Token::IntVal;
            }
        }

    hex:
        c = in.peek();
        if (is_hex_digit(c)) {
            (void)in.get();
            _str_val += c;
            goto hex;
        }
        _int_val = std::stoi(_str_val, nullptr, 16);
        return Token::IntVal;

    octal:
        c = in.peek();
        if (is_octal_digit(c)) {
            (void)in.get();
            _str_val += c;
            goto octal;
        }
        _int_val = std::stoi(_str_val, nullptr, 8);
        return Token::IntVal;

    dec:
        c = in.peek();
        if (isdigit(c)) {
            (void)in.get();
            _str_val += c;
            goto dec;
        }
        _int_val = std::stoi(_str_val, nullptr, 10);
        return Token::IntVal;

    op:
        c = in.peek();
        switch (c) {
            case '=':
            case '>': {
                (void)in.get();
                _str_val += c;
            }
        }
        return _op_map.at(_str_val);

    identifier:
        c = in.peek();
        if (!isspace(c) && is_id_symbol(c)) {
            (void)in.get();
            _str_val += c;
            goto identifier;
        }

        if (_keyword_map.contains(_str_val)) {
            return _keyword_map.at(_str_val);
        }
        return Token::Identifier;
}