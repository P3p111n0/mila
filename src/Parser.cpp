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
    case TokenType::Break:
        return true;
    default:
        return false;
    }
}

Type Parser::Var_type() {
    switch (_lexer.peek().type()) {
    case TokenType::Integer:
        return Type::Int;
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing var type: " + tok.get_str());
        return Type(-1);
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing optional var declaration: " + tok.get_str());
    }
    }
}

ASTNode * Parser::Body() {
    switch (_lexer.peek().type()) {
    case TokenType::Identifier:
    case TokenType::Exit:
    case TokenType::If:
    case TokenType::While:
    case TokenType::For:
    case TokenType::Break:
        /* rule 34: Body_h -> Statement_h */
        return Stmt_helper();
    case TokenType::Begin: {
        /* rule 35: Body_h -> begin Statement end */
        _lexer.match(TokenType::Begin);
        auto stmts = Statement();
        if (!_lexer.match(TokenType::End)) {
            // TODO error
        }
        return new ASTNodeBody(stmts);
    }
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing body: " + tok.get_str());
        return nullptr;
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing assignment: " + tok.get_str());
        return nullptr;
    }
    }
}

ASTNode * Parser::If() {
    switch (_lexer.peek().type()) {
    case TokenType::If: {
        /* rule 40: If -> if Expression then If_body_h If_else_h */
        _lexer.match(TokenType::If);
        ASTNode * cond = Expression();
        if (!_lexer.match(TokenType::Then)) {
            // TODO error
        }
        ASTNode * body = Body();

        switch (_lexer.peek().type()) {
        case TokenType::Else: {
            /* rule 38: If_else_h -> else If_else_h1 */
            _lexer.match(TokenType::Else);
            ASTNode * else_branch = Body();
            return new ASTNodeIf(cond, body, else_branch);
        }
        case TokenType::Semicolon:
        case TokenType::End:
            return new ASTNodeIf(cond, body);
        default: {
            Token tok = _lexer.get();
            _err.emplace_back(tok.pos, "else/end/semicolon expected, got: " + tok.get_str());
            return nullptr;
        }
        }
    }
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing if statement: " + tok.get_str());
        return nullptr;
    }
    }
}

ASTNode * Parser::While() {
    switch (_lexer.peek().type()) {
    case TokenType::While: {
        /* rule 41: While -> while Expression do Body_h */
        _lexer.match(TokenType::While);
        ASTNode * cond = Expression();
        if (!_lexer.match(TokenType::Do)) {
            // TODO error
        }
        ASTNode * body = Body();
        return new ASTNodeWhile(cond, body);
    }
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing while loop: " + tok.get_str());
        return nullptr;
    }
    }
}

ASTNode * Parser::For() {
    switch (_lexer.peek().type()) {
    case TokenType::For: {
        /* rule 44: For -> for Assignment For_to Expression do Body_h */
        _lexer.match(TokenType::For);
        ASTNode * iter_var = Assignment();
        bool is_downto = false;

        switch (_lexer.peek().type()) {
        case TokenType::To:
            /* rule 42: For_to -> to */
            _lexer.match(TokenType::To);
            break;
        case TokenType::Downto: {
            /* rule 43: For_to -> downto */
            _lexer.match(TokenType::Downto);
            is_downto = true;
            break;
        }
        default: {
            Token tok = _lexer.get();
            _err.emplace_back(tok.pos, "to/downto exprected, got: " + tok.get_str());
        }
        }

        ASTNode * iter_stop = Expression();
        if (!_lexer.match(TokenType::Do)) {
            // TODO error
        }
        ASTNode * body = Body();
        return new ASTNodeFor(iter_var, iter_stop, body, is_downto);
    }
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing for loop: " + tok.get_str());
        return nullptr;
    }
    }
}

ASTNode * Parser::Stmt_helper() {
    switch (_lexer.peek().type()) {
    case TokenType::Identifier:
        /* rule 22: Statement_h -> Assignment */
        return Assignment();
    case TokenType::If:
        /* rule 23: Statement_h -> If */
        return If();
    case TokenType::While:
        /* rule 24: Statement_h -> While */
        return While();
    case TokenType::For:
        /* rule 25: Statement_h -> For */
        return For();
    case TokenType::Exit:
        /* rule 27: Statement_h -> exit */
        _lexer.match(TokenType::Exit);
        return new ASTNodeExit();
    case TokenType::Break:
        /* rule 27: Statement_h -> break */
        _lexer.match(TokenType::Break);
        return new ASTNodeBreak();
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing statement: " + tok.get_str());
        return nullptr;
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing statement: " + tok.get_str());
        return {};
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing function variables: " + tok.get_str());
        return {};
    }
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
        _st->functions[id.get_str()]; // reserve function name
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
        old_st->functions[fn.name] = fn; // add incomplete function info
        fn.body = std::shared_ptr<ASTNode>(new ASTNodeBody(Statement()));
        old_st->functions[fn.name] = fn; // update after parsing body
        if (!_lexer.match(TokenType::End)) {
            // TODO error
        }
        if (!_lexer.match(TokenType::Semicolon)) {
            // TODO error
        }
        _st = old_st;
        break;
    }
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing function declaration: " + tok.get_str());
    }
    }
}

void Parser::Procedure() {
    switch (_lexer.peek().type()) {
    case TokenType::Procedure: {
        /* rule 61: Procedure -> procedure Id ( Function_arg ) ; Var_opt begin
         * Statement end ; */
        _lexer.match(TokenType::Procedure);
        Token id = _lexer.get();
        if (id.type() != TokenType::Identifier ||
            !_st->unique_global(id.get_str())) {
            // TODO error
        }
        auto old_st = _st;
        old_st->functions[id.get_str()]; // reserve procedure name
        _st = _st->derive();
        FunctionRecord fn{.name = id.get_str(),
                          .return_type = Type::Void,
                          .symbol_table = _st};
        if (!_lexer.match(TokenType::Par_Open)) {
            // TODO error
        }
        fn.args = Function_arg();
        fn.arity = fn.args.size();
        if (!_lexer.match(TokenType::Par_Close)) {
            // TODO error
        }
        if (!_lexer.match(TokenType::Semicolon)) {
            // TODO error
        }
        Var_optional();
        if (!_lexer.match(TokenType::Begin)) {
            // TODO error
        }
        old_st->functions[fn.name] = fn; // add incomplete procedure info
        fn.body = std::shared_ptr<ASTNode>(new ASTNodeBody(Statement()));
        old_st->functions[fn.name] = fn; // update after parsing body
        if (fn.symbol_table->variables.count(fn.name)) {
            // TODO error - procedure cannot return non void
        }
        if (!_lexer.match(TokenType::End)) {
            // TODO error
        }
        if (!_lexer.match(TokenType::Semicolon)) {
            // TODO error
        }
        _st = old_st;
        break;
    }
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing procedure declaration: " + tok.get_str());
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing constants: " + tok.get_str());
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing constants: " + tok.get_str());
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token parsing variable declaration: " + tok.get_str());
        return {};
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing variable declarations: " + tok.get_str());
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing variables: " + tok.get_str());
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing expressions: " + tok.get_str());
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token add level operators: " + tok.get_str());
        return nullptr;
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token mult level operators: " + tok.get_str());
        return nullptr;
    }
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
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token unary operators: " + tok.get_str());
        return nullptr;
    }
    }
}

ASTNode * Parser::Call(const std::string & name) {
    std::list<std::shared_ptr<ASTNode>> args;
    switch (_lexer.peek().type()) {
    case TokenType::Par_Open: {
        /* rule 69: Call_id -> ( Call_inner ) */
        _lexer.match(TokenType::Par_Open);

        switch (_lexer.peek().type()) {
        case TokenType::Op_Minus:
        case TokenType::Op_Plus:
        case TokenType::Op_Not:
        case TokenType::Identifier:
        case TokenType::IntVal:
        case TokenType::Par_Open: {
            args.emplace_back(Expression());
            while (_lexer.peek().type() == TokenType::Comma) {
                _lexer.match(TokenType::Comma);
                args.emplace_back(Expression());
            }
            break;
        }
        case TokenType::Par_Close:
            break;
        default: {
            Token tok = _lexer.get();
            _err.emplace_back(tok.pos, "Unknown token parsing call args: " + tok.get_str());
            return nullptr;
        }
        }

        _lexer.match(TokenType::Par_Close);
        return new ASTNodeCall(name, args);
    }
    case TokenType::Op_Mul:
    case TokenType::Op_Div:
    case TokenType::Op_Mod:
    case TokenType::Op_And:
    case TokenType::Op_Plus:
    case TokenType::Op_Minus:
    case TokenType::Op_Or:
    case TokenType::Op_Xor:
    case TokenType::Op_Equal:
    case TokenType::Op_NotEqual:
    case TokenType::Op_Lt:
    case TokenType::Op_Gt:
    case TokenType::Op_LtE:
    case TokenType::Op_GtE:
    case TokenType::Par_Close:
    case TokenType::Then:
    case TokenType::Do:
    case TokenType::Semicolon:
    case TokenType::Colon:
    case TokenType::To:
    case TokenType::Downto:
    case TokenType::End: {
        auto var_r = _st->lookup_variable(name);
        auto cst_r = _st->lookup_constant(name);
        if (!var_r.has_value() && !cst_r.has_value()) {
            // TODO error - unbound identifier
        }
        return new ASTNodeIdentifier(name);
    }
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing call: " + tok.get_str());
        return nullptr;
    }
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
    case TokenType::Identifier: {
        Token id = _lexer.get();
        return Call(id.get_str());
    }
    default: {
        Token tok = _lexer.get();
        _err.emplace_back(tok.pos, "Unknown token when parsing factor: " + tok.get_str());
        return nullptr;
    }
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
