{
  open Parser
}

rule tokenise = parse
| [' ' '\t' '\n'] { tokenise lexbuf }
| "type" { TYP }
| "and" { AND }
| "of" { OF }
| "match" { MATCH }
| "with" { WITH }
| ',' { COMMA }
| '=' { EQUAL }
| '|' { BAR }
| '_' { UNDERSCORE }
| "->" { MINUSGREATER }
| '*' { STAR }
| '(' { LPAR }
| ')' { RPAR }
| "'" ['a'-'z']+ as i { TVAR i }
| ['a'-'z' 'A'-'Z']+ as i { IDENT i }
| eof { EOF }