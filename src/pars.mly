%{
  open Base
  let read_schema s = 
    Schema.of_list $ List.init (String.length s) (fun a -> Char.uppercase_ascii (String.get s a))
%}
%token <string> IDENT DECIMAL
%token PLUS ARROW STAR SLASH ZERO NUMERIC CNF INFIX_EQUALITY
%token LPAREN RPAREN NUMERIC  EOL EXCLAMATION LBRACKET MINUS SLASH
%token UNDERSCORE SINGLE_QUOTE DOUBLE_QUOTE VLINE RBRACKET FD_LITERAL
%token DOT W COMMA DOLLAR NEGATED INCLUDE LBRACE RBRACE UNIC_ARROW
%left PLUS LOWER_ALPHAMINUS         /* lowest precedence */
%left STAR SLASH                    /* medium precedence */
%start schema                       /* first entry point */
%start fun_dep                      /* second entry point*/
%type<Base.schema> schema
%type<Base.functional_dep> fun_dep
%%
schema:
 | EOL  	  { Schema.empty   }
 | IDENT EOL  { read_schema $1 }
;


fun_dep:
 | EOL                                        { [] }
 | fun_dep_rec EOL                            { $1 }
 | LBRACE fun_dep_rec RBRACE EOL              { $2 }
 | SLASH LBRACE fun_dep_rec SLASH RBRACE EOL  { $3 }
;

fun_dep_rec:
 | LPAREN IDENT sep IDENT RPAREN                         {  [(read_schema $2  , read_schema $4)]    }
 | LPAREN IDENT sep IDENT RPAREN COMMA  fun_dep_rec      {  (read_schema  $2  , read_schema $4)::$7 }
 | IDENT sep IDENT                                       {  [(read_schema $1  , read_schema $3)]    }
 | IDENT sep IDENT  COMMA  fun_dep_rec                   {  (read_schema  $1  , read_schema $3)::$5 } 
 | FD_LITERAL LBRACE IDENT RBRACE LBRACE IDENT RBRACE    {  [(read_schema $3  , read_schema $6)]    }
 | FD_LITERAL LBRACE IDENT RBRACE LBRACE IDENT RBRACE    
    COMMA  fun_dep_rec              	                 {  (read_schema  $3  , read_schema $6)::$9 }
;

sep: 
 | MINUS       { () }
 | ARROW       { () }
 | UNIC_ARROW  { () }
;