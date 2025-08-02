# Lox grammar (from lowest to highest precedence priority)

program -> declaration* EOF ;

declaration -> classDecl | funcDecl | varDecl | statement;

classDecl -> "class" + IDENTIFIER + ( "<" IDENTIFIER )? +  "{" + function* + "}"
uncDecl -> "fun" function ;
varDecl -> "var" + IDENTIFIER ( "=" expression )? ";" ;
statement -> exprStmt | IfStmt | printstmt | whileStmt | block | returnStmt ;

function -> IDENTIFIER "(" parameters? ")" block ;
parameters -> IDENTIFIER ( "," IDENTIFIER )* ;

exprStmt -> expression ";" ;
forStmt -> "for" "(" ( varDecl | exprStmt | "," ) expression? ";" expression? ")" statement ;
IfStmt -> "if" + "(" + expression + ")" statement ( "else" statement )? ;
printStmt -> "print" expression ";" ;
whileStmt -> "while" "(" expression ")" statement ;
block -> "{" + declaration* + "}" ;
returnStmt -> "return" + expression? + ";" ;

expression -> assignement ;
assignment -> ( call "." )? IDENTIFIER "=" assignement | logic_or ;

logic_or -> logic_and ( "or" logic_and )* ;
logic_and -> equality ( "and" equality )* ;
equality -> comparison ( ("!=" | "==" ) comparison )* ;
comparison -> term ( (">" | ">=" | "<" | "<=") term )* ;
term -> factor ( ("-" | "+" ) factor )* ;
factor -> unary ( ("/" | "*" ) unary )* ;
unary -> ("!" | "-") unary | call ;

call -> primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
arguments -> expression ( "," expression )* ;

primary -> NUMBER | STRING | "true" | "false" | "Nil" | "(" expression ")" | IDENTIFIER | "super" . IDENTIFIER ;
