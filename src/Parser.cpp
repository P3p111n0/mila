#include "Parser.hpp"
#include "ExprEvaluator.hpp"
#include "LambdaLifter.hpp"
#include "TypeChecker.hpp"
#include <memory>

Parser::Parser(std::istream & is)
    : _lexer(is), MilaContext(), MilaBuilder(MilaContext),
      MilaModule("mila", MilaContext), _st(std::make_shared<SymbolTable>()) {

    // init symbol table with lib
    type_ptr int_ty = type_ptr(_tf.get_int_t());
    type_ptr int_ref_ty = type_ptr(new RefType(int_ty));
    type_ptr str_ty = type_ptr(_tf.get_string_t());
    type_ptr double_ty = type_ptr(_tf.get_double_t());
    type_ptr double_ref_ty = type_ptr(new RefType(double_ty));
    FunctionRecord writeln{
        "writeln",
        int_ty,
        {{"x", type_ptr(new MimicType({int_ty, str_ty, double_ty}))}},
        1,
        _st->derive(),
        std::shared_ptr<FnType>(new FnType(
            {type_ptr(new MimicType({int_ty, str_ty, double_ty}))}, int_ty)),
        SymbolTable::make_callsite_ptr()};
    FunctionRecord write{
        "write",
        int_ty,
        {{"x", type_ptr(new MimicType({int_ty, str_ty}))}},
        1,
        _st->derive(),
        std::shared_ptr<FnType>(new FnType(
            {type_ptr(new MimicType({int_ty, str_ty, double_ty}))}, int_ty)),
        SymbolTable::make_callsite_ptr()};
    FunctionRecord readln{
        "readln",
        int_ty,
        {{"x", type_ptr(new MimicType({int_ref_ty, double_ref_ty})), true}},
        1,
        _st->derive(),
        std::shared_ptr<FnType>(new FnType(
            {type_ptr(new MimicType({int_ref_ty, double_ref_ty}))}, int_ty)),
        SymbolTable::make_callsite_ptr()};
    FunctionRecord dec{
        "dec",
        int_ty,
        {{"x", type_ptr(new MimicType({int_ref_ty, double_ref_ty})), true}},
        1,
        _st->derive(),
        std::shared_ptr<FnType>(new FnType(
            {type_ptr(new MimicType({int_ref_ty, double_ref_ty}))}, int_ty)),
        SymbolTable::make_callsite_ptr()};
    FunctionRecord int_cast{
        "to_int",
        int_ty,
        {{"x", type_ptr(new MimicType({int_ty, double_ty})), false}},
        1,
        _st->derive(),
        std::shared_ptr<FnType>(
            new FnType({type_ptr(new MimicType({int_ty, double_ty}))}, int_ty)),
        SymbolTable::make_callsite_ptr()};
    FunctionRecord double_cast{
        "to_double",
        double_ty,
        {{"x", type_ptr(new MimicType({int_ty, double_ty})), false}},
        1,
        _st->derive(),
        std::shared_ptr<FnType>(new FnType(
            {type_ptr(new MimicType({int_ty, double_ty}))}, double_ty)),
        SymbolTable::make_callsite_ptr()};
    _st->functions[writeln.name] = std::move(writeln);
    _st->functions[write.name] = std::move(write);
    _st->functions[readln.name] = std::move(readln);
    _st->functions[dec.name] = std::move(dec);
    _st->functions[int_cast.name] = std::move(int_cast);
    _st->functions[double_cast.name] = std::move(double_cast);
    _builtin_names = {"writeln", "write",  "readln",
                      "dec",     "to_int", "to_double"};
}

void Parser::llvm_init_lib() {
    {
        std::vector<llvm::Type *> Ints(1, llvm::Type::getInt32Ty(MilaContext));
        llvm::FunctionType * FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(MilaContext), Ints, false);
        llvm::Function * F = llvm::Function::Create(
            FT, llvm::Function::ExternalLinkage, "writeln_int", MilaModule);
        for (auto & Arg : F->args())
            Arg.setName("x");
    }
    {
        std::vector<llvm::Type *> Doubles(1,
                                          llvm::Type::getDoubleTy(MilaContext));
        llvm::FunctionType * FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(MilaContext), Doubles, false);
        llvm::Function * F = llvm::Function::Create(
            FT, llvm::Function::ExternalLinkage, "writeln_double", MilaModule);
        for (auto & Arg : F->args())
            Arg.setName("x");
    }
    {
        std::vector<llvm::Type *> PtrToStr(
            1, llvm::Type::getInt8PtrTy(MilaContext));
        llvm::FunctionType * FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(MilaContext), PtrToStr, false);
        llvm::Function * F = llvm::Function::Create(
            FT, llvm::Function::ExternalLinkage, "writeln_string", MilaModule);
        for (auto & Arg : F->args())
            Arg.setName("x");
    }
    {
        std::vector<llvm::Type *> PtrToStr(1,
                                           llvm::Type::getInt32Ty(MilaContext));
        llvm::FunctionType * FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(MilaContext), PtrToStr, false);
        llvm::Function * F = llvm::Function::Create(
            FT, llvm::Function::ExternalLinkage, "write_int", MilaModule);
        for (auto & Arg : F->args())
            Arg.setName("x");
    }
    {
        std::vector<llvm::Type *> Doubles(1,
                                          llvm::Type::getDoubleTy(MilaContext));
        llvm::FunctionType * FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(MilaContext), Doubles, false);
        llvm::Function * F = llvm::Function::Create(
            FT, llvm::Function::ExternalLinkage, "write_double", MilaModule);
        for (auto & Arg : F->args())
            Arg.setName("x");
    }
    {
        std::vector<llvm::Type *> PtrToStr(
            1, llvm::Type::getInt8PtrTy(MilaContext));
        llvm::FunctionType * FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(MilaContext), PtrToStr, false);
        llvm::Function * F = llvm::Function::Create(
            FT, llvm::Function::ExternalLinkage, "write_string", MilaModule);
        for (auto & Arg : F->args())
            Arg.setName("x");
    }
    {
        std::vector<llvm::Type *> IntPtr(
            1, llvm::Type::getInt32PtrTy(MilaContext));
        llvm::FunctionType * FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(MilaContext), IntPtr, false);
        llvm::Function * F = llvm::Function::Create(
            FT, llvm::Function::ExternalLinkage, "readln_int__ref", MilaModule);
        for (auto & Arg : F->args())
            Arg.setName("x");
    }
    {
        std::vector<llvm::Type *> DoublePtr(
            1, llvm::Type::getDoublePtrTy(MilaContext));
        llvm::FunctionType * FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(MilaContext), DoublePtr, false);
        llvm::Function * F =
            llvm::Function::Create(FT, llvm::Function::ExternalLinkage,
                                   "readln_double__ref", MilaModule);
        for (auto & Arg : F->args())
            Arg.setName("x");
    }
    {
        std::vector<llvm::Type *> IntPtr(
            1, llvm::Type::getInt32PtrTy(MilaContext));
        llvm::FunctionType * FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(MilaContext), IntPtr, false);
        llvm::Function * F = llvm::Function::Create(
            FT, llvm::Function::ExternalLinkage, "dec_int__ref", MilaModule);
        for (auto & Arg : F->args())
            Arg.setName("x");
    }
    {
        std::vector<llvm::Type *> DoublePtr(
            1, llvm::Type::getDoublePtrTy(MilaContext));
        llvm::FunctionType * FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(MilaContext), DoublePtr, false);
        llvm::Function * F = llvm::Function::Create(
            FT, llvm::Function::ExternalLinkage, "dec_double__ref", MilaModule);
        for (auto & Arg : F->args())
            Arg.setName("x");
    }
}

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
    case TokenType::Continue:
    case TokenType::Begin:
        return true;
    default:
        return false;
    }
}

std::pair<int, int> Parser::ArrayBounds() {
    if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Br_Open)) {
        _err.emplace_back(tok.pos, "in array decl: '[' expected, got : " +
                                       tok.get_str());
    }
    ExprEvaluator<int> evaluator(_st);
    Position approx_pos = _lexer.peek().pos;
    ast_ptr lower_bound(Expression());
    std::optional<int> lb_opt = evaluator.eval(lower_bound.get());
    if (!lb_opt.has_value()) {
        _err.emplace_back(
            approx_pos,
            "in array decl: lower bound cannot be calculated at compile time.");
    }
    if (auto tok = _lexer.peek(); !_lexer.match(TokenType::DoubleDot)) {
        _err.emplace_back(tok.pos, "in array decl: '..' expected, got : " +
                                       tok.get_str());
    }
    approx_pos = _lexer.peek().pos;
    ast_ptr upper_bound(Expression());
    std::optional<int> ub_opt = evaluator.eval(upper_bound.get());
    if (!ub_opt.has_value()) {
        _err.emplace_back(
            approx_pos,
            "in array decl: upper bound cannot be calculated at compile time.");
    }
    if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Br_Close)) {
        _err.emplace_back(tok.pos, "in array decl: ']' expected, got : " +
                                       tok.get_str());
    }

    if (!lb_opt.has_value() || !ub_opt.has_value()) {
        return {};
    }

    int lb = lb_opt.value();
    int ub = ub_opt.value();

    if (ub <= lb) {
        _err.emplace_back(
            approx_pos,
            "in array decl: upper bound has to be greater than lower bound");
    }

    return {lb_opt.value(), ub_opt.value()};
}

type_ptr Parser::Var_type() {
    switch (_lexer.peek().type()) {
    case TokenType::Integer:
        _lexer.match(TokenType::Integer);
        return type_ptr(_tf.get_int_t());
    case TokenType::Array: {
        _lexer.match(TokenType::Array);
        auto [lower_bound, upper_bound] = ArrayBounds();
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Of)) {
            _err.emplace_back(tok.pos, "in array decl: 'of' expected, got : " +
                                           tok.get_str());
        }
        type_ptr elem_t = Var_type();
        return type_ptr(new ArrayType(elem_t, lower_bound, upper_bound));
    }
    case TokenType::Double:
        _lexer.match(TokenType::Double);
        return type_ptr(_tf.get_double_t());
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos,
                          "Type identifier expected, got: " + tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::Body() {
    switch (_lexer.peek().type()) {
    case TokenType::Identifier:
    case TokenType::Exit:
    case TokenType::If:
    case TokenType::While:
    case TokenType::For:
    case TokenType::Break:
    case TokenType::Continue:
        /* rule 34: Body_h -> Statement_h */
        return Stmt_helper();
    case TokenType::Begin: {
        /* rule 35: Body_h -> begin Statement end */
        _lexer.match(TokenType::Begin);
        auto stmts = Statement();
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::End)) {
            _err.emplace_back(tok.pos,
                              "\'end\' expected, got: " + tok.get_str());
        }
        return ast_ptr(new ASTNodeBody(stmts));
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos,
                          "Unknown token when parsing body: " + tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::If() {
    switch (_lexer.peek().type()) {
    case TokenType::If: {
        /* rule 40: If -> if Expression then If_body_h If_else_h */
        _lexer.match(TokenType::If);
        ast_ptr cond = Expression();
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Then)) {
            _err.emplace_back(tok.pos,
                              "\'then\' expected, got: " + tok.get_str());
        }
        auto old_st = _st;
        _st = _st->derive();
        _st->current_scope = SymbolTable::Scope::If;

        ast_ptr body = Body();

        switch (_lexer.peek().type()) {
        case TokenType::Else: {
            /* rule 38: If_else_h -> else If_else_h1 */
            _lexer.match(TokenType::Else);
            ast_ptr else_branch = Body();
            _st = old_st;
            return ast_ptr(new ASTNodeIf(cond, body, else_branch));
        }
        case TokenType::Semicolon:
        case TokenType::End: {
            _st = old_st;
            return ast_ptr(new ASTNodeIf(cond, body));
        }
        default: {
            _st = old_st;
            Token tok = _lexer.peek();
            _err.emplace_back(tok.pos, "else/end/semicolon expected, got: " +
                                           tok.get_str());
            return nullptr;
        }
        }
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token when parsing if statement: " +
                                       tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::While() {
    switch (_lexer.peek().type()) {
    case TokenType::While: {
        /* rule 41: While -> while Expression do Body_h */
        _lexer.match(TokenType::While);
        ast_ptr cond = Expression();
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Do)) {
            _err.emplace_back(tok.pos,
                              "\'do\' expected, got: " + tok.get_str());
        }
        auto old_st = _st;
        _st = _st->derive();
        _st->current_scope = SymbolTable::Scope::Loop;
        ast_ptr body = Body();
        _st = old_st;
        return ast_ptr(new ASTNodeWhile(cond, body));
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token when parsing while loop: " +
                                       tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::For() {
    switch (_lexer.peek().type()) {
    case TokenType::For: {
        /* rule 44: For -> for Assignment For_to Expression do Body_h */
        _lexer.match(TokenType::For);

        Token id = _lexer.get();
        if (id.type() != TokenType::Identifier) {
            _err.emplace_back(id.pos,
                              "identifier expected, got: " + id.get_str());
        }

        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Op_Assign)) {
            _err.emplace_back(tok.pos,
                              "\':=\' expected, got: " + tok.get_str());
        }

        auto old_st = _st; // create new scope
        _st = _st->derive();
        _st->current_scope = SymbolTable::Scope::Loop;
        _st->variables[id.get_str()] = {
            id.get_str(), std::shared_ptr<Type>(_tf.get_int_t()), false};

        ast_ptr it_start = Expression();

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
            Token tok = _lexer.peek();
            _err.emplace_back(tok.pos, "\'to\'/\'downto\' exprected, got: " +
                                           tok.get_str());
        }
        }

        ast_ptr it_stop = Expression();
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Do)) {
            _err.emplace_back(tok.pos,
                              "\'do\' expected, got: " + tok.get_str());
        }
        ast_ptr body = Body();
        _st = old_st; // restore symbol table
        return ast_ptr(
            new ASTNodeFor(id.get_str(), it_start, it_stop, body, is_downto));
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token when parsing for loop: " +
                                       tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::Stmt_helper() {
    switch (_lexer.peek().type()) {
    case TokenType::Identifier: {
        /* rule 22: Statement_h -> Assignment */
        Token id = _lexer.get();
        std::shared_ptr<ASTNodeAssignable> target_ptr;
        if (_lexer.peek().type() == TokenType::Br_Open) {
            target_ptr = ArrayAccess(id.get_str());
        } else {
            target_ptr = std::shared_ptr<ASTNodeAssignable>(
                new ASTNodeIdentifier(id.get_str()));
        }

        switch (_lexer.peek().type()) {
        case TokenType::Op_Assign: { // assignment
            _lexer.match(TokenType::Op_Assign);
            auto lookup = _st->lookup_variable(id.get_str());
            if (!lookup.has_value()) {
                _err.emplace_back(id.pos,
                                  "unbound identifier: " + id.get_str());
            }
            ast_ptr rhs = Expression();
            return ast_ptr(new ASTNodeAssign(target_ptr, rhs));
        }
        case TokenType::Colon:
        case TokenType::Par_Open: { // call
            return Call(id);
        }
        default: {
            auto tok = _lexer.peek();
            _err.emplace_back(tok.pos,
                              "Unknown token when parsing assignment/call: " +
                                  tok.get_str());
            return nullptr;
        }
        }
    }
    case TokenType::If:
        /* rule 23: Statement_h -> If */
        return If();
    case TokenType::While:
        /* rule 24: Statement_h -> While */
        return While();
    case TokenType::For:
        /* rule 25: Statement_h -> For */
        return For();
    case TokenType::Exit: {
        /* rule 27: Statement_h -> exit */
        auto tok = _lexer.get();
        if (_st->current_scope == SymbolTable::Scope::Global) {
            _err.emplace_back(tok.pos,
                              "\'exit\' not permitted in global scope.");
        }
        return ast_ptr(new ASTNodeExit());
    }
    case TokenType::Break: {
        /* rule 27: Statement_h -> break */
        auto tok = _lexer.get();
        if (!_st->running_in_scope(SymbolTable::Scope::Loop)) {
            _err.emplace_back(tok.pos, "\'break\' used outside of loop.");
        }
        return ast_ptr(new ASTNodeBreak());
    }
    case TokenType::Continue: {
        auto tok = _lexer.get();
        if (!_st->running_in_scope(SymbolTable::Scope::Loop)) {
            _err.emplace_back(tok.pos, "\'continue\' used outside of loop.");
        }
        return ast_ptr(new ASTNodeContinue());
    }
    case TokenType::Begin: {
        auto old_st = _st;
        _st = _st->derive();
        _st->current_scope = old_st->current_scope;
        ast_ptr body = Body();
        _st = old_st;
        return body;
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token when parsing statement: " +
                                       tok.get_str());
        return nullptr;
    }
    }
}

std::list<ast_ptr> Parser::Statement() {
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
        return res;
    }
    case TokenType::End:
        return res;
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token when parsing statement: " +
                                       tok.get_str());
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
        while (_lexer.peek().type() == TokenType::Semicolon) {
            (void)_lexer.get();
            res.emplace_back(Var_declaration());
        }
        return res;
    }
    case TokenType::Par_Close:
        /* rule 59: Function_arg ->  */
        return {};
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos,
                          "Unknown token when parsing function variables: " +
                              tok.get_str());
        return {};
    }
    }
}

ast_ptr Parser::Function() {
    switch (_lexer.peek().type()) {
    case TokenType::Function: {
        /* rule 60: Function -> function Id ( Function_arg ) : Type ; Var_opt
         * begin Statement end ; */
        _lexer.match(TokenType::Function);
        Token id = _lexer.get();
        if (id.type() != TokenType::Identifier) {
            _err.emplace_back(id.pos,
                              "identifier expected, got: " + id.get_str());
        }
        if (!_st->unique_global(id.get_str()) &&
            !_forward_declared.count(id.get_str())) {
            _err.emplace_back(id.pos,
                              "function name is not unique: " + id.get_str());
        }
        auto old_st = _st;
        callsite_ptr callsites;
        if (auto lookup = old_st->lookup_function(id.get_str());
            lookup.has_value()) {
            callsites = lookup.value().callsites;
        } else {
            callsites = SymbolTable::make_callsite_ptr();
        }
        _st->functions[id.get_str()]; // reserve function name
        _st = _st->derive();
        _st->current_scope = SymbolTable::Scope::Function;
        FunctionRecord fn;
        fn.callsites = callsites;
        fn.name = id.get_str(), fn.symbol_table = _st;
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Par_Open)) {
            _err.emplace_back(tok.pos,
                              "in function signature: \'(\' expected, got: " +
                                  tok.get_str());
        }
        fn.args = Function_arg();
        fn.arity = fn.args.size();
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Par_Close)) {
            _err.emplace_back(tok.pos,
                              "in function signature: \')\' expected, got: " +
                                  tok.get_str());
        }
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Colon)) {
            _err.emplace_back(tok.pos,
                              "in function signature: \':\' expected, got: " +
                                  tok.get_str());
        }
        fn.return_type = std::shared_ptr<Type>(Var_type());
        std::vector<std::shared_ptr<Type>> arg_types;
        std::transform(fn.args.begin(), fn.args.end(),
                       std::back_inserter(arg_types),
                       [&](const VariableRecord & v) { return v.type; });
        fn.fn_type = std::make_shared<FnType>(arg_types, fn.return_type);
        // implicit return value variable
        fn.symbol_table->variables[fn.name] = {fn.name, fn.return_type};
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Semicolon)) {
            _err.emplace_back(tok.pos,
                              "in function signature: \';\' expected, got: " +
                                  tok.get_str());
        }

        old_st->functions[fn.name] = fn; // add incomplete function info
        if (_lexer.peek().type() == TokenType::Forward) {
            (void)_lexer.get();
            if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Semicolon)) {
                _err.emplace_back(
                    tok.pos,
                    "in function forward declaration: \';\' expected, got: " +
                        tok.get_str());
            }
            _st = old_st;
            _forward_declared.emplace(fn.name);
            return ast_ptr(
                new ASTNodePrototype(fn.name, fn.args, fn.return_type, true));
        }

        ast_ptr block = Block();
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Begin)) {
            _err.emplace_back(tok.pos,
                              "in function body: \'begin\' expected, got: " +
                                  tok.get_str());
        }
        ast_ptr body(new ASTNodeBody(Statement()));
        old_st->functions[fn.name] = fn; // update after parsing body
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::End)) {
            _err.emplace_back(tok.pos,
                              "in function body: \'end\' expected, got: " +
                                  tok.get_str());
        }
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Semicolon)) {
            _err.emplace_back(tok.pos,
                              "in function body: \';\' expected, got: " +
                                  tok.get_str());
        }
        _st = old_st;
        if (_forward_declared.count(fn.name)) {
            _forward_declared.erase(fn.name);
        }
        std::shared_ptr<ASTNodePrototype> proto(
            new ASTNodePrototype(fn.name, fn.args, fn.return_type, false));
        return ast_ptr(new ASTNodeFunction(proto, block, body));
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos,
                          "Unknown token when parsing function declaration: " +
                              tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::Procedure() {
    switch (_lexer.peek().type()) {
    case TokenType::Procedure: {
        /* rule 61: Procedure -> procedure Id ( Function_arg ) ; Var_opt begin
         * Statement end ; */
        auto fn_sign_pos = _lexer.peek().pos;
        _lexer.match(TokenType::Procedure);
        Token id = _lexer.get();
        if (id.type() != TokenType::Identifier) {
            _err.emplace_back(
                id.pos, "in procedure signature: identifier expected, got: " +
                            id.get_str());
        }
        if (!_st->unique_global(id.get_str())) {
            _err.emplace_back(
                id.pos,
                "in procedure signature: procedure name is not unique: " +
                    id.get_str());
        }
        auto old_st = _st;
        callsite_ptr callsites;
        if (auto lookup = old_st->lookup_function(id.get_str());
            lookup.has_value()) {
            callsites = lookup.value().callsites;
        } else {
            callsites = SymbolTable::make_callsite_ptr();
        }
        old_st->functions[id.get_str()]; // reserve procedure name
        _st = _st->derive();
        _st->current_scope = SymbolTable::Scope::Function;
        FunctionRecord fn{.name = id.get_str(),
                          .return_type =
                              std::shared_ptr<Type>(_tf.get_void_t()),
                          .args = {},
                          .arity = 0,
                          .symbol_table = _st,
                          .fn_type = nullptr,
                          .callsites = callsites};
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Par_Open)) {
            _err.emplace_back(tok.pos,
                              "in procedure signature: \'(\' expected, got: " +
                                  tok.get_str());
        }
        fn.args = Function_arg();
        fn.arity = fn.args.size();
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Par_Close)) {
            _err.emplace_back(tok.pos,
                              "in procedure signature: \')\' expected, got: " +
                                  tok.get_str());
        }
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Semicolon)) {
            _err.emplace_back(tok.pos,
                              "in procedure signature: \';\' expected, got: " +
                                  tok.get_str());
        }

        std::vector<std::shared_ptr<Type>> arg_types;
        std::transform(fn.args.begin(), fn.args.end(),
                       std::back_inserter(arg_types),
                       [&](const VariableRecord & v) { return v.type; });
        fn.fn_type = std::make_shared<FnType>(arg_types, fn.return_type);

        old_st->functions[fn.name] = fn; // add incomplete function info
        if (_lexer.peek().type() == TokenType::Forward) {
            (void)_lexer.get();
            if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Semicolon)) {
                _err.emplace_back(
                    tok.pos,
                    "in procedure forward declaration: \';\' expected, got: " +
                        tok.get_str());
            }
            _st = old_st;
            _forward_declared.emplace(fn.name);
            return ast_ptr(
                new ASTNodePrototype(fn.name, fn.args, fn.return_type, true));
        }

        ast_ptr block = Block();
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Begin)) {
            _err.emplace_back(tok.pos,
                              "in procedure body: \'begin\' expected, got: " +
                                  tok.get_str());
        }
        ast_ptr body(new ASTNodeBody(Statement()));
        old_st->functions[fn.name] = fn; // update after parsing body
        if (fn.symbol_table->variables.count(fn.name)) {
            _err.emplace_back(fn_sign_pos,
                              "Procedure return type cannot be non-void: ");
        }
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::End)) {
            _err.emplace_back(tok.pos,
                              "in procedure body: \'end\' expected, got: " +
                                  tok.get_str());
        }
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Semicolon)) {
            _err.emplace_back(tok.pos,
                              "in procedure body: \';\' expected, got: " +
                                  tok.get_str());
        }
        _st = old_st;
        if (_forward_declared.count(fn.name)) {
            _forward_declared.erase(fn.name);
        }

        std::shared_ptr<ASTNodePrototype> proto(
            new ASTNodePrototype(fn.name, fn.args, fn.return_type, false));
        return ast_ptr(new ASTNodeFunction(proto, block, body));
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos,
                          "Unknown token when parsing procedure declaration: " +
                              tok.get_str());
        return nullptr;
    }
    }
}

ASTNodeConst::ConstExpr Parser::Const_declaration() {
    switch (_lexer.peek().type()) {
    case TokenType::Identifier: {
        auto identifier = _lexer.get();
        if (identifier.type() != TokenType::Identifier) {
            _err.emplace_back(
                identifier.pos,
                "in constant declaration: identifier expected, got: " +
                    identifier.get_str());
        }
        if (_st->lookup_constant(identifier.get_str()).has_value()) {
            _err.emplace_back(
                identifier.pos,
                "in constant declaration: redefinition of constant: " +
                    identifier.get_str());
        }
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Op_Equal)) {
            _err.emplace_back(tok.pos,
                              "in constant declaration: \'=\' expected, got: " +
                                  tok.get_str());
        }
        std::shared_ptr<ASTNode> rhs = std::shared_ptr<ASTNode>(Expression());
        _st->constants[identifier.get_str()] = rhs;
        return {identifier.get_str(), rhs};
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token when parsing constants: " +
                                       tok.get_str());
        return {{}, {}};
    }
    }
}

ast_ptr Parser::Const() {
    std::list<ASTNodeConst::ConstExpr> constants;
    switch (_lexer.peek().type()) {
    case TokenType::Const: {
        /* rule 47: Const -> const Id = Expression ; Const_h */
        _lexer.match(TokenType::Const);
        constants.emplace_back(Const_declaration());
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Semicolon)) {
            _err.emplace_back(tok.pos,
                              "in constant declaration: \';\' expected, got: " +
                                  tok.get_str());
        }
        while (true) {
            switch (_lexer.peek().type()) {
            case TokenType::Identifier: {
                constants.emplace_back(Const_declaration());
                if (auto tok = _lexer.peek();
                    !_lexer.match(TokenType::Semicolon)) {
                    _err.emplace_back(
                        tok.pos,
                        "in constant declaration: \';\' expected, got: " +
                            tok.get_str());
                }
                continue;
            }
            case TokenType::Function:
            case TokenType::Procedure:
            case TokenType::Var:
            case TokenType::Begin:
                return ast_ptr(new ASTNodeConst(constants));
            default: {
                Token tok = _lexer.peek();
                _err.emplace_back(tok.pos,
                                  "Unknown token when parsing constants: " +
                                      tok.get_str());
                return nullptr;
            }
            }
        }
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token when parsing constants: " +
                                       tok.get_str());
        return nullptr;
    }
    }
}

std::list<VariableRecord> Parser::Var_decl_list() {
    std::list<VariableRecord> res;
    std::list<std::string> ids;
    switch (_lexer.peek().type()) {
    case TokenType::Identifier: {
        /* rule 48: Var_decl -> Id : Type */
        Token id = _lexer.get();
        if (!_st->unique_in_current_scope(id.get_str())) {
            _err.emplace_back(
                id.pos, "in variable declaration: redefinition of variable: " +
                            id.get_str());
        }
        ids.emplace_back(id.get_str());
        while (_lexer.peek().type() == TokenType::Comma) {
            (void)_lexer.get();
            Token next_id = _lexer.get();
            if (next_id.type() != TokenType::Identifier) {
                _err.emplace_back(
                    next_id.pos,
                    "in variable declaration: identifier expected, got: " +
                        next_id.get_str());
            }
            if (!_st->unique_in_current_scope(id.get_str())) {
                _err.emplace_back(
                    next_id.pos,
                    "in variable declaration: redefinition of variable: " +
                        next_id.get_str());
            }
            ids.emplace_back(next_id.get_str());
        }
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Colon)) {
            _err.emplace_back(tok.pos,
                              "in variable declaration: \':\' expected, got: " +
                                  tok.get_str());
        }
        std::shared_ptr<Type> var_type = std::shared_ptr<Type>(Var_type());
        std::transform(ids.begin(), ids.end(), std::back_inserter(res),
                       [&](std::string x) {
                           return VariableRecord{std::move(x), var_type, false};
                       });
        for (const auto & var : res) {
            _st->variables[var.name] = var;
        }
        return res;
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos,
                          "Unknown token parsing variable declaration: " +
                              tok.get_str());
        return {};
    }
    }
}

VariableRecord Parser::Var_declaration() {
    switch (_lexer.peek().type()) {
    case TokenType::Identifier: {
        /* rule 48: Var_decl -> Id : Type */
        Token id = _lexer.get();
        if (!_st->unique_in_current_scope(id.get_str())) {
            _err.emplace_back(
                id.pos, "in variable declaration: redefinition of variable: " +
                            id.get_str());
        }
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Colon)) {
            _err.emplace_back(tok.pos,
                              "in variable declaration: \':\' expected, got: " +
                                  tok.get_str());
        }
        std::shared_ptr<Type> var_type = std::shared_ptr<Type>(Var_type());
        VariableRecord var_record = {id.get_str(), var_type};
        _st->variables[id.get_str()] = {id.get_str(), var_type};
        return var_record;
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos,
                          "Unknown token parsing variable declaration: " +
                              tok.get_str());
        return {};
    }
    }
}

ast_ptr Parser::Var() {
    std::list<VariableRecord> vars;
    switch (_lexer.peek().type()) {
    case TokenType::Var: {
        /* rule 51: Var -> var Var_decl ; Var_h */
        _lexer.match(TokenType::Var);
        vars.splice(vars.end(), Var_decl_list());
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Semicolon)) {
            _err.emplace_back(tok.pos,
                              "in variable declaration: \';\' expected, got: " +
                                  tok.get_str());
        }
        while (true) {
            switch (_lexer.peek().type()) {
            case TokenType::Identifier: {
                /* rule 49: Var_h -> Var_decl ; Var_h */
                vars.splice(vars.end(), Var_decl_list());
                if (auto tok = _lexer.peek();
                    !_lexer.match(TokenType::Semicolon)) {
                    _err.emplace_back(
                        tok.pos,
                        "in variable declaration: \';\' expected, got: " +
                            tok.get_str());
                }
                continue;
            }
            case TokenType::Function:
            case TokenType::Procedure:
            case TokenType::Const:
            case TokenType::Begin:
            case TokenType::Var:
                /* rule 50: Var_h ->  */
                return ast_ptr(new ASTNodeVar(vars));
            default: {
                Token tok = _lexer.peek();
                _err.emplace_back(
                    tok.pos,
                    "Unknown token when parsing variable declarations: " +
                        tok.get_str());
                return {};
            }
            }
        }
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token when parsing variables: " +
                                       tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::Expression() {
    switch (_lexer.peek().type()) {
    case TokenType::Op_Minus:
    case TokenType::Op_Plus:
    case TokenType::Op_Not:
    case TokenType::Identifier:
    case TokenType::IntVal:
    case TokenType::DoubleVal:
    case TokenType::StringLiteral:
    case TokenType::Par_Open: {
        /* rule 20: Expression -> Add Expression_h */
        ast_ptr lhs = Add();
        while (is_rel_operator(_lexer.peek().type())) {
            TokenType op = _lexer.get().type();
            ast_ptr rhs = Add();
            switch (op) {
            case TokenType::Op_Equal:
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::Eq));
                break;
            case TokenType::Op_NotEqual:
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::NEq));
                break;
            case TokenType::Op_Lt:
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::Lt));
                break;
            case TokenType::Op_Gt:
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::Gt));
                break;
            case TokenType::Op_LtE:
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::LtE));
                break;
            case TokenType::Op_GtE:
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::GtE));
                break;
            default:
                throw std::runtime_error("expr - this shouldnt happen");
            }
        }
        return lhs;
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token when parsing expressions: " +
                                       tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::Add() {
    switch (_lexer.peek().type()) {
    case TokenType::Op_Minus:
    case TokenType::Op_Plus:
    case TokenType::Op_Not:
    case TokenType::Identifier:
    case TokenType::IntVal:
    case TokenType::DoubleVal:
    case TokenType::StringLiteral:
    case TokenType::Par_Open: {
        /* rule 17: Add -> Mul Add_h */
        ast_ptr lhs = Mul();
        while (is_add_operator(_lexer.peek().type())) {
            TokenType op = _lexer.get().type();
            ast_ptr rhs = Mul();
            switch (op) {
            case TokenType::Op_Plus: {
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::Add));
                break;
            }
            case TokenType::Op_Minus: {
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::Sub));
                break;
            }
            case TokenType::Op_Or: {
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::Or));
                break;
            }
            case TokenType::Op_Xor: {
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::Xor));
                break;
            }
            default:
                throw std::runtime_error("add - this shouldnt happen");
            }
        }
        return lhs;
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token add level operators: " +
                                       tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::Mul() {
    switch (_lexer.peek().type()) {
    case TokenType::Op_Minus:
    case TokenType::Op_Plus:
    case TokenType::Op_Not:
    case TokenType::Identifier:
    case TokenType::IntVal:
    case TokenType::DoubleVal:
    case TokenType::StringLiteral:
    case TokenType::Par_Open: {
        /* rule 14: Mul -> Unary Mul_h */
        ast_ptr lhs = Unary();
        while (is_mul_operator(_lexer.peek().type())) {
            TokenType op = _lexer.get().type();
            ast_ptr rhs = Unary();
            switch (op) {
            case TokenType::Op_Mul: {
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::Mul));
                break;
            }
            case TokenType::Op_Div: {
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::Div));
                break;
            }
            case TokenType::Op_Mod: {
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::Mod));
                break;
            }
            case TokenType::Op_And: {
                lhs = ast_ptr(
                    new ASTNodeBinary(lhs, rhs, ASTNodeBinary::Operator::And));
                break;
            }
            default:
                throw std::runtime_error("mul - this shouldnt happen");
            }
        }
        return lhs;
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token mult level operators: " +
                                       tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::Unary() {
    switch (_lexer.peek().type()) {
    case TokenType::Op_Minus: {
        /* rule 8: Unary -> - Factor */
        _lexer.match(TokenType::Op_Minus);
        ast_ptr arg = Factor();
        return ast_ptr(new ASTNodeBinary(ast_ptr(new ASTNodeInt(-1)), arg,
                                         ASTNodeBinary::Operator::Mul));
    }
    case TokenType::Op_Plus: {
        /* rule 9: Unary -> + Factor */
        _lexer.match(TokenType::Op_Plus);
        return Factor();
    }
    case TokenType::Op_Not: {
        /* rule 10: Unary -> not Factor */
        _lexer.match(TokenType::Op_Not);
        ast_ptr arg = Factor();
        return ast_ptr(new ASTNodeUnary(arg, ASTNodeUnary::Operator::Not));
    }
    case TokenType::Identifier:
    case TokenType::IntVal:
    case TokenType::DoubleVal:
    case TokenType::StringLiteral:
    case TokenType::Par_Open:
        /* rule 11: Unary -> Factor */
        return Factor();
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos,
                          "Unknown token unary operators: " + tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::Call(const Token & id) {
    std::list<std::shared_ptr<ASTNode>> args;
    switch (_lexer.peek().type()) {
    case TokenType::Par_Open: {
        /* rule 69: Call_id -> ( Call_inner ) */
        _lexer.match(TokenType::Par_Open);

        auto fn = _st->lookup_function(id.get_str());
        if (!fn.has_value()) {
            _err.emplace_back(id.pos,
                              "in call: no matching function to call: " +
                                  id.get_str());
        }

        switch (_lexer.peek().type()) {
        case TokenType::Op_Minus:
        case TokenType::Op_Plus:
        case TokenType::Op_Not:
        case TokenType::Identifier:
        case TokenType::IntVal:
        case TokenType::DoubleVal:
        case TokenType::StringLiteral:
        case TokenType::Par_Open: {
            const std::list<VariableRecord> & vars =
                fn.has_value() ? fn.value().args : std::list<VariableRecord>();
            std::size_t i = 0;
            auto it = vars.begin();
            if (!vars.empty() && it->pass_by_ref) {
                args.emplace_back(VarByRef());
                ++i;
                if (i != vars.size()) {
                    ++it;
                }
            } else {
                args.emplace_back(Expression());
            }

            while (_lexer.peek().type() == TokenType::Comma) {
                _lexer.match(TokenType::Comma);
                if (it->pass_by_ref) {
                    args.emplace_back(VarByRef());
                } else {
                    args.emplace_back(Expression());
                }
                if (i != vars.size()) {
                    ++i;
                    ++it;
                }
            }
            break;
        }
        case TokenType::Par_Close:
            break;
        default: {
            Token tok = _lexer.peek();
            _err.emplace_back(tok.pos, "Unknown token parsing call args: " +
                                           tok.get_str());
            return nullptr;
        }
        }

        if (fn.has_value() && fn.value().arity != args.size()) {
            _err.emplace_back(id.pos,
                              "in call: arity mismatch: " + id.get_str());
        }

        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Par_Close)) {
            _err.emplace_back(tok.pos,
                              "in call: \')\' expected, got: " + tok.get_str());
        }
        if (_builtin_names.contains(id.get_str())) {
            return ast_ptr(new ASTNodeBuiltinCall(id.get_str(), args));
        }
        std::shared_ptr<ASTNodeCall> ptr(new ASTNodeCall(id.get_str(), args));
        _st->add_callsite(id.get_str(), ptr);
        return ptr;
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
    case TokenType::Br_Close:
    case TokenType::Then:
    case TokenType::Do:
    case TokenType::Semicolon:
    case TokenType::Comma:
    case TokenType::To:
    case TokenType::Downto:
    case TokenType::Else:
    case TokenType::End: {
        auto var_r = _st->lookup_variable(id.get_str());
        auto cst_r = _st->lookup_constant(id.get_str());
        if (!var_r.has_value() && !cst_r.has_value()) {
            _err.emplace_back(id.pos, "unbound identifier: " + id.get_str());
        }
        if (var_r.has_value() && cst_r.has_value()) {
            _err.emplace_back(id.pos, "name is ambiguous: " + id.get_str());
        }

        return ast_ptr(new ASTNodeIdentifier(id.get_str()));
    }
    case TokenType::Colon: {
        _lexer.match(TokenType::Colon);
        if (!_st->unique_in_current_scope(id.get_str())) {
            _err.emplace_back(id.pos,
                              "in local variable declaration: identifier not "
                              "unique in current scope: " +
                                  id.get_str());
        }
        std::shared_ptr<Type> type = std::shared_ptr<Type>(Var_type());
        _st->variables[id.get_str()] = {id.get_str(), type, false};
        return ast_ptr(new ASTNodeVar({{id.get_str(), type}}));
    }
    case TokenType::Br_Open: {
        return ArrayAccess(id.get_str());
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos,
                          "Unknown token when parsing call: " + tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::VarByRef() {
    switch (_lexer.peek().type()) {
    case TokenType::Identifier: {
        Token tok = _lexer.get();
        std::string id = tok.get_str();

        auto lookup = _st->lookup_variable(id);

        if (!lookup.has_value()) {
            _err.emplace_back(tok.pos, "unbound identifier: " + id);
        } else if (!lookup.has_value() &&
                   _st->lookup_constant(id).has_value()) {
            _err.emplace_back(
                tok.pos,
                "cannot pass a mutable reference to a constant: " + id);
        } else if (_lexer.peek().type() == TokenType::Par_Open) {
            _err.emplace_back(
                tok.pos, "cannot pass a mutable reference to function: " + id);
        }

        std::shared_ptr<ASTNodeAssignable> ptr;
        if (_lexer.peek().type() == TokenType::Br_Open) {
            ptr = ArrayAccess(id);
        } else {
            ptr = std::shared_ptr<ASTNodeAssignable>(new ASTNodeIdentifier(id));
        }

        return ast_ptr(new ASTNodeVarByRef(ptr));
    }
    default:
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos,
                          "Unknown token when parsing variable reference: " +
                              tok.get_str());
        return nullptr;
    }
}

std::shared_ptr<ASTNodeAssignable>
Parser::ArrayAccess(const std::string & name) {
    std::vector<std::shared_ptr<ASTNode>> idx;
    switch (_lexer.peek().type()) {
    case TokenType::Br_Open: {
        while (_lexer.peek().type() == TokenType::Br_Open) {
            _lexer.match(TokenType::Br_Open);
            ast_ptr expr = Expression();
            if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Br_Close)) {
                _err.emplace_back(tok.pos,
                                  "in array access: ']' expected, got: " +
                                      tok.get_str());
            }
            idx.emplace_back(expr);
        }
        return std::shared_ptr<ASTNodeAssignable>(
            new ASTNodeArrAccess(name, idx));
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token when parsing array acces: " +
                                       tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::Factor() {
    switch (_lexer.peek().type()) {
    case TokenType::Par_Open: { /* rule 4: Factor -> ( Expression ) */
        _lexer.match(TokenType::Par_Open);
        ast_ptr expr = Expression();
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Par_Close)) {
            _err.emplace_back(tok.pos, "\')\' expected, got: " + tok.get_str());
        }
        return expr;
    }
    case TokenType::IntVal: {
        auto token = _lexer.get();
        return ast_ptr(new ASTNodeInt(token.get_int()));
    }
    case TokenType::DoubleVal: {
        auto token = _lexer.get();
        return ast_ptr(new ASTNodeDouble(token.get_double()));
    }
    case TokenType::StringLiteral: {
        auto token = _lexer.get();
        return ast_ptr(new ASTNodeString(token.get_str()));
    }
    case TokenType::Identifier: {
        Token id = _lexer.get();
        return Call(id);
    }
    default: {
        Token tok = _lexer.peek();
        _err.emplace_back(tok.pos, "Unknown token when parsing factor: " +
                                       tok.get_str());
        return nullptr;
    }
    }
}

ast_ptr Parser::Block() {
    std::list<std::shared_ptr<ASTNode>> declarations;
    while (true) {
        switch (_lexer.peek().type()) {
        case TokenType::Const:
            declarations.emplace_back(Const());
            continue;
        case TokenType::Var:
            declarations.emplace_back(Var());
            continue;
        case TokenType::Function:
            declarations.emplace_back(Function());
            continue;
        case TokenType::Procedure:
            declarations.emplace_back(Procedure());
            continue;
        case TokenType::Begin:
            return ast_ptr(new ASTNodeBlock(declarations));
        default: {
            auto tok = _lexer.peek();
            _err.emplace_back(tok.pos, "Unknown token when parsing block: " +
                                           tok.get_str());
            return nullptr;
        }
        }
    }
}

ast_ptr Parser::Mila() {
    switch (_lexer.peek().type()) {
    case TokenType::Program: {
        /* rule 1: Mila -> program Id ; Const_opt Var_opt Fn_prod begin
         * Statement end . */
        _lexer.match(TokenType::Program);
        Token pg_name = _lexer.get();
        _st->current_scope = SymbolTable::Scope::Global;
        if (pg_name.type() != TokenType::Identifier) {
            _err.emplace_back(pg_name.pos,
                              "in main: program name expected, got: " +
                                  pg_name.get_str());
        }
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Semicolon)) {
            _err.emplace_back(tok.pos,
                              "in main: \';\' expected, got: " + tok.get_str());
        }
        ast_ptr block = Block();
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Begin)) {
            _err.emplace_back(tok.pos, "in main: \'begin\' expected, got: " +
                                           tok.get_str());
        }
        ast_ptr main_body(new ASTNodeBody(Statement()));
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::End)) {
            _err.emplace_back(tok.pos, "in main: \'end\' expected, got: " +
                                           tok.get_str());
        }
        if (auto tok = _lexer.peek(); !_lexer.match(TokenType::Dot)) {
            _err.emplace_back(tok.pos,
                              "in main: \'.\' at main end expected, got: " +
                                  tok.get_str());
        }

        if (!_forward_declared.empty()) {
            for (const auto & fn : _forward_declared) {
                _err.emplace_back(Position(),
                                  "function definition missing: " + fn);
            }
        }

        std::shared_ptr<ASTNodePrototype> proto(new ASTNodePrototype(
            "main", {}, std::shared_ptr<Type>(_tf.get_int_t()), false));
        return ast_ptr(new ASTNodeFunction(proto, block, main_body));
    }
    default: {
        auto tok = _lexer.peek();
        _err.emplace_back(tok.pos, "in main: \'program\' expected, got: " +
                                       tok.get_str());
        return nullptr;
    }
    }
}

bool Parser::Parse() {
    _current_code = std::shared_ptr<ASTNode>(Mila());
    if (!_err.empty()) {
        for (const auto & error : _err) {
            std::cerr << error << std::endl;
        }
        return false;
    }
    LambdaLifter ll(_st);
    ll.lift_tree(_current_code);

    TypeChecker tc(_st);
    _current_code = tc.tree_rebuild(_current_code);
    if (!_current_code) {
        tc.errs(std::cerr);
        return false;
    }
    return true;
}

const llvm::Module & Parser::Generate() {
    llvm_init_lib();
    CodegenData data;
    _current_code->codegen(MilaModule, MilaBuilder, MilaContext, data);
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
