{
open Core
open Lexing
open Parser

type error =
  | Illegal_character of char
  | Invalid_literal of string

exception Lexer_error of error * Location.t

(* The table of keywords *)

let keyword_table = Hashtbl.of_alist_exn (module String) [
  ("fun", FUN); (* lambda *)
  ("let", LET);
  ("in", IN);
  ("end", END);

  ("unit", TRIVIAL);
  ("Unit", UNIT);

  ("true", TRUE);
  ("false", FALSE);  
  ("and", AND);
  ("or", OR);
  ("if", IF);
  ("then", THEN);
  ("else", ELSE);
  ("bool", BOOL);

  ("fst", FST);
  ("snd", SND);
  (* ("nil", NIL); *)
  (* ("cons", CONS); *)
  (* ("list", LIST); *)
  ("inl", INL);
  ("inr", INR);
  ("case", CASE);
  ("of", OF);
  ("|", BAR);
  ("=>", RightArrow);

  ("nat", NAT);
  ("preal", PREAL);
  ("real", REAL);
  ("ureal", UREAL);

  ("external", EXTERNAL);
  ("type", TYPE);
  ("def", DEF);

  ("BERN", BERN);
  ("BETA", BETA);
  ("BIN", BIN);
  ("CAT", CAT);
  ("GAMMA", GAMMA);
  ("GEO", GEO);
  ("NORMAL", NORMAL);
  ("POIS", POIS);
  ("UNIF", UNIF);

  ("inf", INF);
  ("logPr", LOGPR);
  ("at", AT);
  ("logML", LOGML);

  ("return", RETURN);
  ("sample", SAMPLE);
  ("choose", CHOOSE);
  ("observe", OBSERVE);
  ("from", FROM);
  ("factor", FACTOR);
  ("dist", DIST);

  ("*", ASTERISK);
  (":", COLON);
  (".", DOT);
  ("=", EQUAL);
  (">", GREATER);
  (">=", GREATEREQUAL);
  ("{", LBRACE);
  ("[", LBRACKET);
  ("<", LESS);
  ("!=", LESSGREATER);
  ("<=", LESSEQUAL);
  ("(", LPAREN);
  ("-", MINUS);
  ("->", MINUSGREATER);
  ("+", PLUS);
  ("}", RBRACE);
  ("]", RBRACKET);
  (")", RPAREN);
  (";", SEMI);
  ("/", SLASH);
  (",", COMMA);
]

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

(* Error report *)

let error lexbuf e = raise (Lexer_error (e, Location.curr lexbuf))

let prepare_error loc = function
  | Illegal_character c ->
    Location.errorf ~loc "illegal character (%s)" (Char.escaped c)
  | Invalid_literal s ->
    Location.errorf ~loc "invalid literal %s" s

let () =
  Location.register_error_of_exn
    (function
      | Lexer_error (err, loc) -> Some (prepare_error loc err)
      | _ -> None
    )
}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let decimal_literal = ['0'-'9'] ['0'-'9' '_']*
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal = '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal = '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal = decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?

rule token = parse
  | newline
    { update_loc lexbuf None 1 false 0; token lexbuf }
  | blank+
    { token lexbuf }
  | "_"
    {IDENT "_" }
  | lowercase identchar* as name
    { match Hashtbl.find keyword_table name with
      | Some kwd -> kwd
      | None -> IDENT name }
  | uppercase identchar* as name
    { match Hashtbl.find keyword_table name with
      | Some kwd -> kwd
      | None -> IDENT name }
  | int_literal as lit
    { INTV (Int.of_string lit) }
  | (float_literal | hex_float_literal) as lit
    { FLOATV (Float.of_string lit) }
  | (float_literal | hex_float_literal | int_literal) identchar+ as invalid
    { error lexbuf (Invalid_literal invalid) }
  | "//"
    { comment lexbuf }
  | "(*"
    { multil_line_comment 0 lexbuf }
  | "=>" | ">=" | "!=" | "<=" | "<-" | "->" | "-o" | "/\\" | "<|" | "|>" | "<+>"
    { Hashtbl.find_exn keyword_table (lexeme lexbuf) }
  | ['&' '*' '|' ':' '$' '.' '=' '>' '{' '[' '<' '(' '-' '+' '}' ']' ')' ';' '/' '#' ',' '?']
    { Hashtbl.find_exn keyword_table (lexeme lexbuf) }
  | eof
    { EOF }
  | _ as illegal_char
    { error lexbuf (Illegal_character illegal_char) }

and comment = parse
  | newline
    { update_loc lexbuf None 1 false 0; token lexbuf }
  | _
    { comment lexbuf }

and multil_line_comment comment_level = parse
  | newline
    { 
      update_loc lexbuf None 1 false 0;
      multil_line_comment comment_level lexbuf
    }
  | "(*" 
    { multil_line_comment (comment_level + 1) lexbuf }
  | "*)"
   { 
    if comment_level > 0 then
      multil_line_comment (comment_level - 1) lexbuf
    else if comment_level = 0 then 
      token lexbuf
    else 
       failwith "Unmatched closing comment"
    } 
  | _
    { multil_line_comment comment_level lexbuf }
    