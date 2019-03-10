{

open Tokens_sig
open Tokens_mod
open Tokens
open Errormsg

module Scanner = struct

let linenum = ErrorMsg.linenum
let linepos = ErrorMsg.linepos

}

let digits = ['0' - '9']
let chars = ['a' - 'z' 'A' - 'Z']

rule scan = parse
  | "/*"
    { comments 1 lexbuf }
  | '"'
    { strings (Buffer.create 8) (Lexing.lexeme_start lexbuf) lexbuf }
  | "if"
    { (IF (Lexing.lexeme_start lexbuf), lexbuf) }
  | "then"
    { (THEN (Lexing.lexeme_start lexbuf), lexbuf) }
  | "else"
    { (ELSE (Lexing.lexeme_start lexbuf), lexbuf) }
  | "let"
    { (LET (Lexing.lexeme_start lexbuf), lexbuf) }
  | "nil"
    { (NIL (Lexing.lexeme_start lexbuf), lexbuf) }
  | "type"
    { (TYPE (Lexing.lexeme_start lexbuf), lexbuf) }
  | "var"
    { (VAR (Lexing.lexeme_start lexbuf), lexbuf) }
  | "of"
    { (OF (Lexing.lexeme_start lexbuf), lexbuf) }
  | "array"
    { (ARRAY (Lexing.lexeme_start lexbuf), lexbuf) }
  | "end"
    { (END (Lexing.lexeme_start lexbuf), lexbuf) }
  | "in"
    { (IN (Lexing.lexeme_start lexbuf), lexbuf) }
  | "function"
    { (FUNCTION (Lexing.lexeme_start lexbuf), lexbuf) }
  | "while"
    { (WHILE (Lexing.lexeme_start lexbuf), lexbuf) }
  | "do"
    { (DO (Lexing.lexeme_start lexbuf), lexbuf) }
  | "for"
    { (FOR (Lexing.lexeme_start lexbuf), lexbuf) }
  | "to"
    { (TO (Lexing.lexeme_start lexbuf), lexbuf) }   

   
  | "+" 
    { (PLUS (Lexing.lexeme_start lexbuf), lexbuf) }
  | "-" 
    { (MINUS (Lexing.lexeme_start lexbuf), lexbuf) }
  | "*" 
    { (TIMES (Lexing.lexeme_start lexbuf), lexbuf) }
  | "/" 
    { (DIVIDE (Lexing.lexeme_start lexbuf), lexbuf) }
  | "&" 
    { (AND (Lexing.lexeme_start lexbuf), lexbuf) }
  | "|" 
    { (OR (Lexing.lexeme_start lexbuf), lexbuf) }
  | "," 
    { (COMMA (Lexing.lexeme_start lexbuf), lexbuf) }
  | ":=" 
    { (ASSIGN (Lexing.lexeme_start lexbuf), lexbuf) }
  | ":"
    { (COLON (Lexing.lexeme_start lexbuf), lexbuf) }
  | "="
    { (EQ (Lexing.lexeme_start lexbuf), lexbuf) }
  | "<>" 
    { (NEQ (Lexing.lexeme_start lexbuf), lexbuf) }
  | ">" 
    { (GT (Lexing.lexeme_start lexbuf), lexbuf) }
  | "<" 
    { (LT (Lexing.lexeme_start lexbuf), lexbuf) }
  | ">=" 
    { (GE (Lexing.lexeme_start lexbuf), lexbuf) }
  | "<=" 
    { (LE (Lexing.lexeme_start lexbuf), lexbuf) }
  | "." 
    { (DOT (Lexing.lexeme_start lexbuf), lexbuf) }
  | ";" 
    { (SEMICOLON (Lexing.lexeme_start lexbuf), lexbuf) }
  | "(" 
    { (LPAREN (Lexing.lexeme_start lexbuf), lexbuf) }
  | ")" 
    { (RPAREN (Lexing.lexeme_start lexbuf), lexbuf) }
  | "[" 
    { (LBRACK (Lexing.lexeme_start lexbuf), lexbuf) }
  | "]" 
    { (RBRACK (Lexing.lexeme_start lexbuf), lexbuf) }
  | "{" 
    { (LBRACE (Lexing.lexeme_start lexbuf), lexbuf) }
  | "}" 
    { (RBRACE (Lexing.lexeme_start lexbuf), lexbuf) }


  | digits+
    { (INT (int_of_string (Lexing.lexeme lexbuf), 
            (Lexing.lexeme_start lexbuf)), lexbuf) }


    
    
  | chars (chars | digits | '_')*
    { (ID (Lexing.lexeme lexbuf, 
            (Lexing.lexeme_start lexbuf)), lexbuf) }


  | '\n'
    { linenum := !linenum + 1;
      linepos := Lexing.lexeme_start lexbuf :: !linepos;
      scan lexbuf }
  | " "
    { scan lexbuf }
  | "\t"
    { scan lexbuf }
  | eof
    { (EOF (Lexing.lexeme_start lexbuf), lexbuf) }
  | _ 
    { ErrorMsg.error (Lexing.lexeme_start lexbuf) "illegal character";
      scan lexbuf }
      
and strings str posi = parse
  | '"'
    { (STRING ((Buffer.contents str), 
            posi), lexbuf) }
  | '\n'
    { linenum := !linenum + 1;
      linepos := Lexing.lexeme_start lexbuf :: !linepos;
      Buffer.add_char str '\n';
      strings str posi lexbuf }
  | '\\''n'
    { Buffer.add_char str '\n';
      strings str posi lexbuf  }
  | '\\''t'
    { Buffer.add_char str '\t';
      strings str posi lexbuf  }
  | '\\''r'
    { Buffer.add_char str '\r';
      strings str posi lexbuf  }
      
  | '\\' (digits digits digits as c)
    { let ch = int_of_string c in
      if ch <= 127 then
        Buffer.add_char str (Char.chr ch)
      else
        ErrorMsg.error (Lexing.lexeme_start lexbuf) "\\num more than 127";
        strings str posi lexbuf}
      
  | '\\' '^' (['@' - '_'] as c)
    { let ch = Char.chr ((Char.code c) - 64) in
      Buffer.add_char str ch;
      strings str posi lexbuf }
  
  | '\\' ('\n' | ' ' | '\t' | '\r')* '\\'
    { strings str posi lexbuf }
  | '\\' '\\'
    { Buffer.add_char str '\\';
      strings str posi lexbuf }
  | _ as c
    { Buffer.add_char str c;
      strings str posi lexbuf } 
  | eof
    { ErrorMsg.error (Lexing.lexeme_start lexbuf) "eof in string";
     strings str posi lexbuf }

and comments nested = parse
  | "/*" 
    { comments (nested + 1) lexbuf }
  | "*/" 
    { if nested = 1 then scan lexbuf
      else comments (nested - 1) lexbuf }
  | _
    { comments nested lexbuf }
  | "\n" 
    { linenum := !linenum + 1;
      linepos := Lexing.lexeme_start lexbuf :: !linepos;
      comments nested lexbuf }
  | eof
    { ErrorMsg.error (Lexing.lexeme_start lexbuf) "eof in com";
     comments nested lexbuf }


{ 
(* end of the scanner module *)
end
}

