# Logic language DSL

This repo explores creating a DSL (*metalanguage*) for describing logic DSLs

Unlike the usual way when people build ever 
more sophisticated interpreters out of simple ones,
here we try a different approach:
Make a complicated piece of ...

BNF for the metalanguage
```bnf
Language ::= ValuesDef BasisDefs
Law ::= "Commutative" | "Associative" | "HasIdentity" Val | "SelfDual"
LawsDef ::= "Laws:" {Law";"}
BasisDefs ::= {Func ":" FuncDef "#" LawsDef ";"}
Func ::= [a-zA-Z_]([a-zA-Z_0-9])*
ValuesDef ::= "Values:" Val (","Val)*
Val ::= ([a-Z0-9])+
```