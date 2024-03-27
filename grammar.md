# Mila Grammar 

Keywords: `program`, `const`, `var`, `integer`, `begin`, `end`, `if`, `while`, `do`, `for`,      `function`, `procedure`, `then`, `exit`, `div`, `mod`, `and`, `or`, `break`. 

Helper symbols:
```
Octal_prefix -> 0
	     -> &

Hex_prefix   -> 0x
	     -> 0X 
	     -> $ 

Digit        -> 0 | ... | 9
Hex_digit    -> A | ... | F | a | ... | f | Digit
Octal_digit  -> 0 | ... | 7 

Dec_number   -> Digit 
	     -> Digit Dec_number
Hex_number   -> Hex_digit
	     -> Hex_digit Hex_number
Octal_number -> Octal_digit
	     -> Octal_digit Octal_number

Id_symbol    -> a | ... | z | A | ... | Z | _
```

Elementary symbols:
```
Integer    -> Dec_number
	   -> Hex_prefix Hex_number
	   -> Octal_prefix Octal_number

Identifier -> Id_symbol
	   -> Id_symbol Identifier
```

Operators based on their priority:
```
Mul_op -> *
       -> /
       -> div
       -> mod
       -> and
  
Add_op -> +
       -> -
       -> or
       -> xor

Rel_op -> =
       -> <>
       -> <
       -> >
       -> <=
       -> >=
```

The Mila language grammar:
```
Mila         -> program Identifier ; 
			Const_opt 
			Var_opt
			Fn_prod
			begin Statement end .

Factor       -> Identifier Call_id 
	     -> Integer 
	     -> ( Expression ) 
  
Unary 	     -> - Factor
             -> + Factor
	     -> not Factor
	     -> Factor
  
Mul_h 	     -> Mul_op Unary Mul_h
	     ->
Mul	     -> Unary Mul_h
  
Add_h 	     -> Add_op Mul Add_h
	     -> 
Add	     -> Mul Add_h

Expression_h -> Rel_op Add Expression_h
             -> 
Expression   -> Add Expression_h

Assignment   -> Identifier := Expression 

Statement_h  -> Assignment 
 	     -> If 
	     -> While 
	     -> For 
	     -> Call 
	     -> exit
	     -> break
Stmt_1       -> ; Stmt_dup
             ->
Stmt_dup     -> Statement_h Stmt_1
             ->
Statement    -> Statement_h Stmt_1

Type         -> integer

Body_h	     -> Statement_h
	     -> begin Statement end

If_else_h    -> else Body_h
	     ->
If           -> if Expression then
		   Body_h
	           If_else_h

While        -> while Expression do Body_h

For_to       -> to
             -> downto
For          -> for Assignment For_to Expression do Body_h

Const_h      -> Identifier = Expression ; Const_h
             -> 
Const        -> const Identifier = Expression ; Const_h

Var_decl     -> Identifier : Type
Var_h	     -> Var_decl ; Var_h
	     -> 
Var          -> var Var_decl ; Var_h

Var_opt	     -> Var
	     ->

Const_opt    -> Const
	     ->

Fn_arg_h     -> , Var_decl Fn_arg_h
             ->
Function_arg -> Var_decl Fn_arg_h
	     ->
Function     -> function Identifier ( Function_arg ) : Type ;
	                 Var_opt
	                 begin Statement end ;

Procedure    -> procedure Identifier ( Function_arg ) ;
			  Var_opt
			  begin Statement end ;

Fn_prod	     -> Function Fn_prod
	     -> Procedure Fn_prod
             ->

Call_h	     -> , Expression
	     -> 
Call_inner   -> Expression Call_h
	     ->
Call_id      -> ( Call_inner )
             ->
```
