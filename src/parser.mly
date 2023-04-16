

%token<string> IDENT
%token<string> TVAR
%token LPAR RPAR COMMA EQUAL BAR MINUSGREATER STAR
%token TYP AND OF
%token EOF

%right MINUSGREATER

%type<Syntax.Type.t> simple_typ
%type<Syntax.Type.t> typ

%type<Syntax.Type.constr> constr1_decl
%type<Syntax.Type.decl> typ1_def

%start<Syntax.Type.defs> program

%%

program: i = impl EOF { i }

impl: TYP d = typ_decl { d }

typ_decl:
  | f = typ1_decl AND s = typ_decl { f :: s }
  | d = typ1_decl { [d] }

typ_params:
  | LPAR ts = separated_nonempty_list(COMMA, TVAR) RPAR { ts }
  | t = TVAR { [t] }
  | /* epsilon */ { [] }

typ1_decl: ts = typ_params c = IDENT d = typ1_def { (ts, c, d) }

typ1_def:
  | EQUAL option(BAR) cs = constr_decl { Syntax.Type.Variant cs }

constr_decl: cs = separated_nonempty_list(BAR, constr1_decl) { cs }

constr1_decl:
  | c = IDENT OF t = typ { Valued (c, t) } 
  | c = IDENT { Unital c }

typ:
  | t = simple_typ { t }
  | ts = typ_star_list { Prod (List.rev ts) }
  | l = typ MINUSGREATER r = typ { Arrow (l, r) }

simple_typ:
  | tv = TVAR { Var tv }
  | t = IDENT { Ctor ([], t) }
  | a = simple_typ t = IDENT { Ctor ([a], t) }
  | LPAR hd = typ COMMA tl = separated_nonempty_list(COMMA, typ) RPAR t = IDENT
    { Ctor (hd :: tl, t) }
  | LPAR t = typ RPAR { t }

typ_star_list:
  | ts = typ_star_list STAR t = simple_typ { t :: ts }
  | l = simple_typ STAR r = simple_typ { [r; l] }

