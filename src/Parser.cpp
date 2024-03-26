#include "Parser.hpp"

Parser::Parser()
    : MilaContext(), MilaBuilder(MilaContext), MilaModule("mila", MilaContext),
      _st(std::make_shared<SymbolTable>()) {}

bool Parser::is_mul_operator(TokenType t) {
    switch (t) {
    case TokenType::Op_Mul:
    case TokenType::Op_Div:
    case TokenType::Op_Mod:
    case TokenType::Op_And:
        return true;
    default:
        return false;
    }
}

bool Parser::is_add_operator(TokenType t) {
    switch (t) {
    case TokenType::Op_Plus:
    case TokenType::Op_Minus:
    case TokenType::Op_Or:
    case TokenType::Op_Xor:
        return true;
    default:
        return false;
    }
}

bool Parser::is_rel_operator(TokenType t) {
    switch (t) {
    case TokenType::Op_Equal:
    case TokenType::Op_NotEqual:
    case TokenType::Op_Lt:
    case TokenType::Op_Gt:
    case TokenType::Op_LtE:
    case TokenType::Op_GtE:
        return true;
    default:
        return false;
    }
}

bool Parser::is_statement(TokenType t) {
    switch (t) {
    case TokenType::Identifier:
    case TokenType::Exit:
    case TokenType::If:
    case TokenType::While:
    case TokenType::For:
        return true;
    default:
        return false;
    }
}

Type Parser::Var_type() {
    switch (_lexer.peek().type()) {
    case TokenType::Integer:
        return Type::Int;
    default:
        throw std::runtime_error("type - unknown type"); // TODO error
    }
}

void Parser::Var_optional() {
    switch (_lexer.peek().type()) {
    case TokenType::Var:
        /* rule 52: Var_opt -> Var */
        Var();
        break;
    case TokenType::Function:
    case TokenType::Procedure:
    case TokenType::Begin:
        /* rule 53: Var_opt ->  */
        break;
    default:
        throw std::runtime_error("ahoj");
    }
}

ASTNode * Parser::Assignment() {
    switch (_lexer.peek().type()) {
    case TokenType::Identifier: {
        /* rule 21: Assignment -> Id := Expression */
        Token id = _lexer.get();
        if (!_lexer.match(TokenType::Op_Assign)) {
            // TODO error
        }
        ASTNode * rhs = Expression();
        return new ASTNodeAssign(new ASTNodeIdentifier(id.get_str()), rhs);
    }
    default:
        throw std::runtime_error("ahoj");
    }
}

ASTNode * Parser::Stmt_helper() {
    switch (_lexer.peek().type()) {
    case TokenType::Identifier:
        /* rule 22: Statement_h -> Assignment */
        return Assignment();
    case TokenType::If:
        /* rule 23: Statement_h -> If */
        If();
        break;
    case TokenType::While:
        /* rule 24: Statement_h -> While */
        While();
        break;
    case TokenType::For:
        /* rule 25: Statement_h -> For */
        For();
        break;
    case TokenType::Exit:
        /* rule 27: Statement_h -> exit */
        _lexer.match(TokenType::Exit);
        return new ASTNodeExit();
    default:
        throw std::runtime_error("ahoj");
    }
}

std::list<std::shared_ptr<ASTNode>> Parser::Statement() {
    std::list<std::shared_ptr<ASTNode>> res;
    res.emplace_back(Stmt_helper());

    switch (_lexer.peek().type()) {
    case TokenType::Semicolon: {
        _lexer.match(TokenType::Semicolon);
        while (is_statement(_lexer.peek().type())) {
            res.emplace_back(Stmt_helper());
            if (_lexer.peek().type() == TokenType::Semicolon) {
                _lexer.match(TokenType::Semicolon);
            }
        }
        if (!_lexer.match(TokenType::End)) {
            // TODO error
        }
        return res;
    }
    case TokenType::End:
        return res;
    default:
        throw std::runtime_error("ahoj");
    }
}

std::list<VariableRecord> Parser::Function_arg() {
    std::list<VariableRecord> res;
    switch (_lexer.peek().type()) {
    case TokenType::Identifier: {
        /* rule 58: Function_arg -> Var_decl Fn_arg_h */
        res.emplace_back(Var_declaration());
        while (_lexer.peek().type() == TokenType::Comma) {
            (void)_lexer.get();
            res.emplace_back(Var_declaration());
        }
        if (!_lexer.match(TokenType::Par_Close)) {
            // TODO error
        }
        return res;
    }
    case TokenType::Par_Close:
        /* rule 59: Function_arg ->  */
        return {};
    default:
        throw std::runtime_error("ahoj");
    }
}

void Parser::Function() {
    switch (_lexer.peek().type()) {
    case TokenType::Function: {
        /* rule 60: Function -> function Id ( Function_arg ) : Type ; Var_opt
         * begin Statement end ; */
        _lexer.match(TokenType::Function);
        Token id = _lexer.get();
        if (id.type() != TokenType::Identifier ||
            !_st->unique_global(id.get_str())) {
            // TODO error
        }
        _st->functions[id.get_str()];
        auto old_st = _st;
        _st = _st->derive();
        FunctionRecord fn;
        fn.name = id.get_str(), fn.symbol_table = _st;
        if (!_lexer.match(TokenType::Par_Open)) {
            // TODO error
        }
        fn.args = Function_arg();
        fn.arity = fn.args.size();
        if (!_lexer.match(TokenType::Par_Close)) {
            // TODO error
        }
        if (!_lexer.match(TokenType::Colon)) {
            // TODO error
        }
        fn.return_type = Var_type();
        // implicit return value variable
        fn.symbol_table->variables[fn.name] = {fn.name, fn.return_type};
        if (!_lexer.match(TokenType::Semicolon)) {
            // TODO error
        }
        Var_optional();
        if (!_lexer.match(TokenType::Begin)) {
            // TODO error
        }
        fn.body = std::shared_ptr<ASTNode>(new ASTNodeBody(Statement()));
        if (!_lexer.match(TokenType::End)) {
            // TODO error
        }
        if (!_lexer.match(TokenType::Semicolon)) {
            // TODO error
        }
        _st = old_st;
        break;
    }
    default:
        throw std::runtime_error("ahoj");
    }
}

void Parser::Const_recursive() {
    switch (_lexer.peek().type()) {
    case TokenType::Identifier: {
        /* rule 45: Const_h -> Id = Expression ; Const_h */
        auto identifier = _lexer.get();
        if (identifier.type() != TokenType::Identifier ||
            _st->constants.count(identifier.get_str())) {
            // TODO error
        }
        _lexer.match(TokenType::Op_Equal);
        ASTNode * lhs = Expression();
        if (!_lexer.match(TokenType::Semicolon)) {
            // TODO error
        }
        _st->constants[identifier.get_str()] = std::shared_ptr<ASTNode>(lhs);
        Const_recursive();
        break;
    }
    case TokenType::Var:
    case TokenType::Function:
    case TokenType::Procedure:
    case TokenType::Begin:
        /* rule 46: Const_h ->  */
        break;
    default:
        throw std::runtime_error("ahoj");
    }
}

void Parser::Const() {
    switch (_lexer.peek().type()) {
    case TokenType::Const: {
        /* rule 47: Const -> const Id = Expression ; Const_h */
        _lexer.match(TokenType::Const);
        auto identifier = _lexer.get();
        if (identifier.type() != TokenType::Identifier ||
            !_st->unique_in_current_scope(identifier.get_str())) {
            // TODO error
        }
        _lexer.match(TokenType::Op_Equal);
        ASTNode * lhs = Expression();
        if (!_lexer.match(TokenType::Semicolon)) {
            // TODO error
        }
        _st->constants[identifier.get_str()] = std::shared_ptr<ASTNode>(lhs);
        Const_recursive();
        break;
    }
    default:
        throw std::runtime_error("ahoj");
    }
}

VariableRecord Parser::Var_declaration() {
    switch (_lexer.peek().type()) {
    case TokenType::Identifier: {
        /* rule 48: Var_decl -> Id : Type */
        Token id = _lexer.get();
        if (!_st->unique_in_current_scope(id.get_str())) {
            // TODO error
        }
        if (!_lexer.match(TokenType::Colon)) {
            // TODO error
        }
        Type var_type = Var_type();
        _st->variables[id.get_str()] = {id.get_str(), var_type};
        break;
    }
    default:
        throw std::runtime_error("ahoj");
    }
}

void Parser::Var_recursive() {
    switch (_lexer.peek().type()) {
    case TokenType::Identifier:
        /* rule 49: Var_h -> Var_decl ; Var_h */
        (void)Var_declaration();
        if (!_lexer.match(TokenType::Semicolon)) {
            // TODO error
        }
        Var_recursive();
        break;
    case TokenType::Function:
    case TokenType::Procedure:
    case TokenType::Begin:
        /* rule 50: Var_h ->  */
        break;
    default:
        throw std::runtime_error("ahoj");
    }
}

void Parser::Var() {
    switch (_lexer.peek().type()) {
    case TokenType::Var:
        /* rule 51: Var -> var Var_decl ; Var_h */
        _lexer.match(TokenType::Var);
        Var_declaration();
        _lexer.match(TokenType::Semicolon);
        Var_recursive();
        break;
    default:
        throw std::runtime_error("ahoj");
    }
}

ASTNode * Parser::Expression() {
    switch (_lexer.peek().type()) {
    case TokenType::Op_Minus:
    case TokenType::Op_Plus:
    case TokenType::Op_Not:
    case TokenType::Identifier:
    case TokenType::IntVal:
    case TokenType::Par_Open: {
        /* rule 20: Expression -> Add Expression_h */
        ASTNode * lhs = Add();
        while (is_rel_operator(_lexer.peek().type())) {
            TokenType op = _lexer.get().type();
            ASTNode * rhs = Add();
            switch (op) {
            case TokenType::Op_Equal:
                lhs = new ASTNodeEq(lhs, rhs);
                break;
            case TokenType::Op_NotEqual:
                lhs = new ASTNodeNotEq(lhs, rhs);
                break;
            case TokenType::Op_Lt:
                lhs = new ASTNodeLt(lhs, rhs);
                break;
            case TokenType::Op_Gt:
                lhs = new ASTNodeGt(lhs, rhs);
                break;
            case TokenType::Op_LtE:
                lhs = new ASTNodeLtE(lhs, rhs);
                break;
            case TokenType::Op_GtE:
                lhs = new ASTNodeGtE(lhs, rhs);
                break;
            default:
                throw std::runtime_error("expr - this shouldnt happen");
            }
        }
        return lhs;
    }
    default:
        throw std::runtime_error("ahoj");
    }
}

ASTNode * Parser::Add() {
    switch (_lexer.peek().type()) {
    case TokenType::Op_Minus:
    case TokenType::Op_Plus:
    case TokenType::Op_Not:
    case TokenType::Identifier:
    case TokenType::IntVal:
    case TokenType::Par_Open: {
        /* rule 17: Add -> Mul Add_h */
        ASTNode * lhs = Mul();
        while (is_add_operator(_lexer.peek().type())) {
            TokenType op = _lexer.get().type();
            ASTNode * rhs = Mul();
            switch (op) {
            case TokenType::Op_Plus: {
                lhs = new ASTNodeAdd(lhs, rhs);
                break;
            }
            case TokenType::Op_Minus: {
                lhs = new ASTNodeSub(lhs, rhs);
                break;
            }
            case TokenType::Op_Or: {
                lhs = new ASTNodeOr(lhs, rhs);
                break;
            }
            case TokenType::Op_Xor: {
                lhs = new ASTNodeXor(lhs, rhs);
                break;
            }
            default:
                throw std::runtime_error("add - this shouldnt happen");
            }
        }
        return lhs;
    }
    default:
        throw std::runtime_error("ahoj");
    }
}

ASTNode * Parser::Mul() {
    switch (_lexer.peek().type()) {
    case TokenType::Op_Minus:
    case TokenType::Op_Plus:
    case TokenType::Op_Not:
    case TokenType::Identifier:
    case TokenType::IntVal:
    case TokenType::Par_Open: {
        /* rule 14: Mul -> Unary Mul_h */
        ASTNode * lhs = Unary();
        while (is_mul_operator(_lexer.peek().type())) {
            TokenType op = _lexer.get().type();
            ASTNode * rhs = Unary();
            switch (op) {
            case TokenType::Op_Mul: {
                lhs = new ASTNodeMul(lhs, rhs);
                break;
            }
            case TokenType::Op_Div: {
                lhs = new ASTNodeDiv(lhs, rhs);
                break;
            }
            case TokenType::Op_Mod: {
                lhs = new ASTNodeMod(lhs, rhs);
                break;
            }
            case TokenType::Op_And: {
                lhs = new ASTNodeAnd(lhs, rhs);
                break;
            }
            default:
                throw std::runtime_error("mul - this shouldnt happen");
            }
        }
        return lhs;
    }
    default:
        throw std::runtime_error("ahoj");
    }
}

ASTNode * Parser::Unary() {
    switch (_lexer.peek().type()) {
    case TokenType::Op_Minus: {
        /* rule 8: Unary -> - Factor */
        _lexer.match(TokenType::Op_Minus);
        ASTNode * arg = Factor();
        return new ASTNodeUnaryMinus(arg);
    }
    case TokenType::Op_Plus: {
        /* rule 9: Unary -> + Factor */
        _lexer.match(TokenType::Op_Plus);
        return Factor();
    }
    case TokenType::Op_Not: {
        /* rule 10: Unary -> not Factor */
        _lexer.match(TokenType::Op_Not);
        ASTNode * arg = Factor();
        return new ASTNodeNot(arg);
    }
    case TokenType::Identifier:
    case TokenType::IntVal:
    case TokenType::Par_Open:
        /* rule 11: Unary -> Factor */
        return Factor();
    default:
        throw std::runtime_error("ahoj");
    }
}

ASTNode * Parser::Factor() {
    switch (_lexer.peek().type()) {
    case TokenType::Par_Open: { /* rule 4: Factor -> ( Expression ) */
        _lexer.match(TokenType::Par_Open);
        Expression();
        _lexer.match(TokenType::Par_Close);
        break;
    }
    case TokenType::IntVal: {
        auto token = _lexer.get();
        return new ASTNodeInt(token.get_int());
    }
    case TokenType::Identifier:
        // identifier
        break;

    default:
        throw std::runtime_error("ahoj");
    }
}

bool Parser::Parse() {
    // getNextToken();
    return true;
}

const llvm::Module & Parser::Generate() {

    // create writeln function
    {
        std::vector<llvm::Type *> Ints(1, llvm::Type::getInt32Ty(MilaContext));
        llvm::FunctionType * FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(MilaContext), Ints, false);
        llvm::Function * F = llvm::Function::Create(
            FT, llvm::Function::ExternalLinkage, "writeln", MilaModule);
        for (auto & Arg : F->args())
            Arg.setName("x");
    }

    // create main function
    {
        llvm::FunctionType * FT =
            llvm::FunctionType::get(llvm::Type::getInt32Ty(MilaContext), false);
        llvm::Function * MainFunction = llvm::Function::Create(
            FT, llvm::Function::ExternalLinkage, "main", MilaModule);

        // block
        llvm::BasicBlock * BB =
            llvm::BasicBlock::Create(MilaContext, "entry", MainFunction);
        MilaBuilder.SetInsertPoint(BB);

        // call writeln with value from lexel
        MilaBuilder.CreateCall(
            MilaModule.getFunction("writeln"),
            {llvm::ConstantInt::get(MilaContext,
                                    llvm::APInt(32, _lexer.numVal()))});

        // return 0
        MilaBuilder.CreateRet(
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(MilaContext), 0));
    }

    return this->MilaModule;
}

/**
 * @brief Simple token buffer.
 *
 * CurTok is the current token the parser is looking at
 * getNextToken reads another token from the lexer and updates curTok with ts
 * result Every function in the parser will assume that CurTok is the cureent
 * token that needs to be parsed
 */
// Token Parser::getNextToken() { return CurTok = _lexer.get(); }
