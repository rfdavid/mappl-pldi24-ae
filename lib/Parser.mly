%{
open AbstractSyntaxTree
open Type
let mkloc = Location.mkloc

let make_loc (start_pos, end_pos) = {
  Location.loc_start = start_pos;
  Location.loc_end = end_pos;
  Location.loc_ghost = false;
}

let mkbty ~loc bty_desc = {
  bty_desc;
  bty_loc = make_loc loc;
}

let mkexp ~loc exp_desc = {
  exp_desc;
  exp_loc = make_loc loc;
}

let mktrm ~loc trm_desc = {
  trm_desc;
  trm_loc = make_loc loc;
  trm_type = None;
}

let mkcmd ~loc cmd_desc = {
  cmd_desc;
  cmd_loc = make_loc loc;
  cmd_type = None;
}
%}

%token FUN LET IN END

%token TRIVIAL UNIT
%token TRUE FALSE AND OR IF THEN ELSE BOOL

%token FST SND
%token INL INR CASE OF BAR RightArrow

%token NAT REAL PREAL UREAL

%token EXTERNAL TYPE DEF

%token BERN BETA BIN CAT GAMMA GEO NORMAL POIS UNIF

%token INF LOGPR AT LOGML

%token RETURN SAMPLE CHOOSE OBSERVE FROM FACTOR
%token DIST

%token ASTERISK
%token COLON
%token DOT
%token EQUAL
%token GREATER
%token GREATEREQUAL
%token LBRACE
%token LBRACKET
%token LESS
%token LESSGREATER
%token LESSEQUAL
%token LPAREN
%token MINUS
%token MINUSGREATER
%token PLUS
%token RBRACE
%token RBRACKET
%token RPAREN
%token SEMI
%token SLASH
%token COMMA

%token <float> FLOATV
%token <int> INTV
%token <string> IDENT
%token EOF

%nonassoc RightArrow

%right OR
%right AND
%nonassoc EQUAL LESSGREATER LESS LESSEQUAL GREATER GREATEREQUAL
%left PLUS MINUS
%left ASTERISK SLASH

%start implementation
%type <AbstractSyntaxTree.prog> implementation

%%

%inline mkloc(symb): symb { mkloc $1 (make_loc $sloc) }
%inline mkcty(symb): symb { mkcty ~loc:$sloc $1 }
%inline mktty(symb): symb { mktty ~loc:$sloc $1 }
%inline mkbty(symb): symb { mkbty ~loc:$sloc $1 }
%inline mkexp(symb): symb { mkexp ~loc:$sloc $1 }
%inline mktrm(symb): symb { mktrm ~loc:$sloc $1 }
%inline mkcmd(symb): symb { mkcmd ~loc:$sloc $1 }

%public implementation:
  | prog = list(toplevel); EOF
    { prog }

// DOUBLESEMI:
//   SEMI;SEMI
//   {}

// ENDOFTOP:
//   option(DOUBLESEMI)
//   {}

toplevel:
  | TYPE; ty_name = mkloc(IDENT); EQUAL; ty = base_ty; 
    { Top_type (ty_name, ty) }
  | DEF; pure_name = mkloc(IDENT); COLON; bty = base_ty; EQUAL; pure_body = exp; 
    { Top_pure (pure_name, bty, pure_body) }
  | DEF; proc_name = mkloc(IDENT); proc_sig = proc_sig; EQUAL; proc_body = cmd; 
    { Top_proc (proc_name, { proc_sig; proc_body; proc_loc = make_loc $sloc }) }
  | EXTERNAL; TYPE; ty_name = mkloc(IDENT); 
    { Top_external_type (ty_name) }
  | EXTERNAL; DEF; var_name = mkloc(IDENT); COLON; ty = base_ty; 
    { Top_external_pure (var_name, ty) }

proc_sig:
  | LPAREN; psig_arg_tys = separated_list(COMMA, param_ty); RPAREN; COLON; psig_ret_ty = base_ty;
    { { psig_arg_tys; psig_ret_ty } }

param_ty:
  | var_name = mkloc(IDENT); COLON; bty = base_ty
    { (var_name, bty) }

base_ty:
  | bty = base_sum_prod_ty
    { bty }
  | mkbty(
      bty1 = base_sum_prod_ty; MINUSGREATER; bty2 = base_ty
      { Bty_arrow (bty1, bty2) }
    )
    { $1 }

base_sum_prod_ty:
  | bty = base_prim_ty
    { bty }
  | mkbty(
       bty1 = base_prim_ty; PLUS; bty2 = base_sum_prod_ty
      { Bty_sum (bty1, bty2) }
      | bty1 = base_prim_ty; ASTERISK; bty2 = base_sum_prod_ty
      { Bty_prod (bty1, bty2) }
    )
    { $1 }

base_prim_ty:
  | LPAREN; bty = base_ty; RPAREN
    { bty }
  | mkbty(
      pty = prim_ty
      { Bty_prim pty }
    | bty = base_prim_ty; DIST
      { Bty_dist bty }
    | type_name = mkloc(IDENT)
      { Bty_var type_name }
    )
    { $1 }

prim_ty:
  | UNIT
    { Pty_unit }
  | BOOL
    { Pty_bool }
  | UREAL
    { Pty_ureal }
  | PREAL
    { Pty_preal }
  | REAL
    { Pty_real }
  | NAT; LBRACKET; n = INTV; RBRACKET
    { Pty_fnat n }
  | NAT
    { Pty_nat }

exp:
  | mkexp(
        IF; cond = exp; THEN; exp_then = exp; ELSE; exp_else = exp; END
      { E_cond (cond, exp_then, exp_else) }
      | CASE; exp = exp; OF; BAR; INL; lvar_name = mkloc(IDENT); RightArrow; left_exp = exp; BAR; INR; rvar_name = mkloc(IDENT); RightArrow;; right_exp = exp; END
      { E_case (exp, lvar_name, left_exp, rvar_name, right_exp) }
      | FUN; LPAREN; arg_name=mkloc(IDENT); COLON; arg_type=base_ty; RPAREN; RightArrow; body = exp
      { E_abs (arg_name, arg_type, body) }
    )
  { $1 }
  | exp = prim_exp
    { exp }
  | exp = app_exp
    { exp }
  | exp = arith_exp
    { exp }

arith_exp:
  | mkexp(
        MINUS; n = INTV
      { E_real (Float.of_int (-n)) }
      | MINUS; r = FLOATV
      { E_real (-.r) }
      | exp1 = exp; bop = mkloc(bop); exp2 = exp
      { E_binop (bop, exp1, exp2) }
      | INF
        { E_inf }
      | MINUS; INF
        { E_ninf }
    )
    { $1 }

%inline bop:
  | PLUS
    { Bop_add }
  | MINUS
    { Bop_sub }
  | ASTERISK
    { Bop_mul }
  | SLASH
    { Bop_div }
  | EQUAL
    { Bop_eq }
  | LESSGREATER
    { Bop_ne }
  | LESS
    { Bop_lt }
  | LESSEQUAL
    { Bop_le }
  | GREATER
    { Bop_gt }
  | GREATEREQUAL
    { Bop_ge }
  | AND
    { Bop_and }
  | OR
    { Bop_or }

app_exp:
  | mkexp(
        rator = app_exp; rand = prim_exp
      { E_app (rator, rand) }
      | rator = prim_exp; rand = prim_exp
      { E_app (rator, rand) }
      | FST; LPAREN; rand = prim_exp; RPAREN
      { E_inl (rand) }
      | SND; LPAREN; rand = prim_exp; RPAREN
      { E_inr (rand) }
    )
    { $1 }

prim_exp:
  | LPAREN; exp = exp; RPAREN
    { exp }
  | mkexp(
      var_name = mkloc(IDENT)
      { E_var var_name }
    | TRIVIAL
      { E_triv }
    | TRUE
      { E_bool true }
    | FALSE
      { E_bool false }
    | n = INTV
      { E_nat n }
    | r = FLOATV
      { E_real r }
    | LET; var_name = mkloc(IDENT); EQUAL; exp1 = exp; IN; exp2 = exp; END
      { E_let (exp1, var_name, exp2) }
    | dist = dist(exp)
      { E_dist dist }
    | LPAREN; exp1 = exp; COMMA; exp2 = exp; RPAREN
      { E_pair (exp1, exp2) }
    | exp = prim_exp; DOT; field = INTV
      { E_field (exp, field) }
    | LOGPR; dist_exp = exp; AT; val_exp = exp; END
    { E_logPr (dist_exp, val_exp)}
    | LOGML; LPAREN; cmd = cmd; RPAREN
    { E_logML (cmd)}
    )
    { $1 }

dist(RHS):
  | BERN; LPAREN; arg = RHS; RPAREN
    { D_ber arg }
  | UNIF
    { D_unif }
  | BETA; LPAREN; arg1 = RHS; COMMA; arg2 = RHS; RPAREN
    { D_beta (arg1, arg2) }
  | GAMMA; LPAREN; arg1 = RHS; COMMA; arg2 = RHS; RPAREN
    { D_gamma (arg1, arg2) }
  | NORMAL; LPAREN; arg1 = RHS; COMMA; arg2 = RHS; RPAREN
    { D_normal (arg1, arg2) }
  | CAT; LPAREN; args = separated_nonempty_list(COMMA, RHS); RPAREN
    { D_cat args }
  | BIN; LPAREN; n = INTV; COMMA; arg = RHS; RPAREN
    { D_bin (n, arg) }
  | GEO; LPAREN; arg = RHS; RPAREN
    { D_geo arg }
  | POIS; LPAREN; arg = RHS; RPAREN
    { D_pois arg }

trm:
  | mktrm(
        RETURN; exp = exp;
      { T_ret exp }
      | SAMPLE; LPAREN; exp = exp; RPAREN
      { T_sample exp }
      | FACTOR; LPAREN; exp = exp; RPAREN
      { T_factor exp }
      | OBSERVE val_exp = exp; FROM; dist_exp = exp;
      { T_observe (dist_exp, val_exp)}
      | IF; exp = exp; THEN; cmd1 = cmd; ELSE; cmd2 = cmd; END
      { T_branch (exp, cmd1, cmd2) }
      | CASE; exp = exp; OF; BAR; INL; lvar_name = mkloc(IDENT); RightArrow; left_cmd = cmd; BAR; INR; rvar_name = mkloc(IDENT); RightArrow;; right_cmd = cmd; END
      { T_case (exp, lvar_name, left_cmd, rvar_name, right_cmd) }
      | proc_name = mkloc(IDENT); LPAREN; args = separated_list(COMMA, exp); RPAREN
      { T_call (proc_name, args) }
      | CHOOSE LPAREN; lb = exp; COMMA; ub = exp; RPAREN
      { T_choose (lb, ub) }
    )
    { $1 }

cmd:
  | LBRACE; cmd = cmd; RBRACE
    { cmd }
  | mkcmd(
      trm = trm; 
      { M_trm trm }
    |  trm = trm; SEMI; cmd = cmd
      { M_seq (trm, cmd) }
    | var_name = mkloc(IDENT); EQUAL; trm = trm; SEMI; cmd = cmd
      { M_bnd (var_name, trm, cmd) }
    )
    { $1 }
