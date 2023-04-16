{
  open Parser
}

rule tokenise = parse
| [' ' '\t' '\n'] { tokenise lexbuf }
| "type" { TYP }
| "and" { AND }
| "of" { OF }
| ',' { COMMA }
| '=' { EQUAL }
| '|' { BAR }
| "->" { MINUSGREATER }
| '*' { STAR }
| '(' { LPAR }
| ')' { RPAR }
| "'" ['a'-'z']+ as i { TVAR i }
| ['a'-'z' 'A'-'Z']+ as i { IDENT i }
| eof { EOF }