{
open Normform_pars     (* The type token is defined in parser.mli *)
exception Eof
}
let alpha = (['a' - 'z'] | ['A' - 'Z'] )+

rule token = parse
  | [ '#' '%' ] [^'\n']*     { token lexbuf   }
  | [' ' '\t' '\n' '\r' ]    { token lexbuf   }     (* skip blanks *)
  | "W"                      { W              }
  | alpha  as l              { IDENT l        }
  | '\''                     { SINGLE_QUOTE   }
  | '"'                      { DOUBLE_QUOTE   }
  | '$'                      { DOLLAR         }
  | '_'                      { UNDERSCORE     }
  | '-'                      { MINUS          }
  | '~'                      { NEGATED        }
  | "!"                      { EXCLAMATION    }
  | '='                      { INFIX_EQUALITY }
  | '|'                      { VLINE          }
  | '.'                      { DOT            }
  | '['                      { LBRACKET       }
  | ']'                      { RBRACKET       }
  | '('                      { LPAREN         }
  | ')'                      { RPAREN         }
  | '{'                      { LBRACE         }
  | '}'                      { RBRACE         }
  | ','                      { COMMA          }
  | '+'                      { PLUS           }
  | "->"                     { ARROW          }
  | "→"                      { UNIC_ARROW     }
  | eof                      { EOL            }