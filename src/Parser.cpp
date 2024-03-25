#include "Parser.hpp"

Parser::Parser()
    : MilaContext(), MilaBuilder(MilaContext), MilaModule("mila", MilaContext) {
}

bool Parser::is_mul_operator(TokenType t) const {
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

ASTNode * Parser::Mul() {
    switch(_lexer.peek().type()) {
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
    }
    default:
        throw std::runtime_error("ahoj");
    }
}

ASTNode * Parser::Unary() {
    switch(_lexer.peek().type()) {
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
    switch(_lexer.peek().type()) {
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
    //getNextToken();
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
//Token Parser::getNextToken() { return CurTok = _lexer.get(); }
