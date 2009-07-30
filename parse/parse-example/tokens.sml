
structure Tokens =
struct

    (* print[hello [b[world]]!] 

       lexes as

       ID "print"
       TEXT [STR "hello ",
             EXP (ID "b" :: TEXT [STR "world"])
             STR "!"]

       *)

    datatype text =
        EXP of (token * Pos.pos) Stream.stream
      | STR of string

    and token =
        ID of string
      | INT of IntConst.intconst
      | CHAR of char
      | FLOAT of real
      | TEXT of text list
      | BAD of char
      | FN
      | LET
      | IN
      | OP
      | OPEN
      | END
      | CASE
      | OF
      | AS
      | ELSE
      | IF
      | THEN
      | TIMES
      | DIVIDE
      | HASH
      | DO
      | VAL
      | AND
      | FUN
      | TYPE
      | DATATYPE
      | DERIVING
      | EXCEPTION
      | VEXCEPTION
      | HANDLE
      | TAGTYPE
      | NEWTAG
      | NEWVTAG
      | ANDALSO
      | ANDTHEN
      | ORELSE
      | OTHERWISE
      | INFIX
      | INFIXR
      | NONFIX
      | STRUCTURE
      | STRUCT
      | LIBRARY
      | LIB
      | SIGNATURE
      | SIG
      | COLONGREATER
      | DOT
      | LPAREN
      | RPAREN
      | DARROW
      | ARROW
      | COLON
      | SEMICOLON
      | LBRACE
      | RAISE
      | RBRACE
      | BAR
      | UNDERSCORE
      | EQUALS
      | COMMA
      | IMPORT
      | DATAFILE

      | PRIMAPP
      | INLINE

      (* ML5-specific *)
      | EXTERN
      | EXPORT
      | GET
      | PUT
      | AT
      | FROM
      | WORLD
      | TILDE
      | ADDR
      | UNIT
      | HOLD
      | LETA
      | LETSHAM
      | SHAM

      | JAVASCRIPT
      | BYTECODE

      | LETCC
      | THROW
      | TO
      | SAY


    (* only for "basic" tokens, not constant-wrappers *)
    fun eq (FN, FN) = true
      | eq (LET, LET) = true
      | eq (IN, IN) = true
      | eq (END, END) = true
      | eq (CASE, CASE) = true
      | eq (OF, OF) = true
      | eq (OPEN, OPEN) = true
      | eq (AS, AS) = true
      | eq (ELSE, ELSE) = true
      | eq (IF, IF) = true
      | eq (THEN, THEN) = true
      | eq (TIMES, TIMES) = true
      | eq (DIVIDE, DIVIDE) = true
      | eq (HASH, HASH) = true
      | eq (DO, DO) = true
      | eq (VAL, VAL) = true
      | eq (AND, AND) = true
      | eq (FUN, FUN) = true
      | eq (TYPE, TYPE) = true
      | eq (DATATYPE, DATATYPE) = true
      | eq (EXCEPTION, EXCEPTION) = true
      | eq (VEXCEPTION, VEXCEPTION) = true
      | eq (HANDLE, HANDLE) = true
      | eq (TAGTYPE, TAGTYPE) = true
      | eq (NEWTAG, NEWTAG) = true
      | eq (NEWVTAG, NEWVTAG) = true
      | eq (ANDALSO, ANDALSO) = true
      | eq (ANDTHEN, ANDTHEN) = true
      | eq (ORELSE, ORELSE) = true
      | eq (OTHERWISE, OTHERWISE) = true
      | eq (INFIX, INFIX) = true
      | eq (INFIXR, INFIXR) = true
      | eq (NONFIX, NONFIX) = true
      | eq (STRUCTURE, STRUCTURE) = true
      | eq (STRUCT, STRUCT) = true
      | eq (LIBRARY, LIBRARY) = true
      | eq (LIB, LIB) = true
      | eq (SIGNATURE, SIGNATURE) = true
      | eq (SIG, SIG) = true
      | eq (COLONGREATER, COLONGREATER) = true
      | eq (RAISE, RAISE) = true
      | eq (DOT, DOT) = true
      | eq (LPAREN, LPAREN) = true
      | eq (RPAREN, RPAREN) = true
      | eq (DARROW, DARROW) = true
      | eq (ARROW, ARROW) = true
      | eq (COLON, COLON) = true
      | eq (SEMICOLON, SEMICOLON) = true
      | eq (LBRACE, LBRACE) = true
      | eq (RBRACE, RBRACE) = true
      | eq (OP, OP) = true
      | eq (BAR, BAR) = true
      | eq (UNDERSCORE, UNDERSCORE) = true
      | eq (DERIVING, DERIVING) = true
      | eq (EQUALS, EQUALS) = true
      | eq (COMMA, COMMA) = true
      | eq (IMPORT, IMPORT) = true
      | eq (LETCC, LETCC) = true
      | eq (THROW, THROW) = true
      | eq (TO, TO) = true
      | eq (DATAFILE, DATAFILE) = true
      | eq (AT, AT) = true
      | eq (GET, GET) = true
      | eq (EXTERN, EXTERN) = true
      | eq (FROM, FROM) = true
      | eq (WORLD, WORLD) = true
      | eq (TILDE, TILDE) = true
      | eq (ADDR, ADDR) = true
      | eq (EXPORT, EXPORT) = true
      | eq (UNIT, UNIT) = true
      | eq (PRIMAPP, PRIMAPP) = true
      | eq (JAVASCRIPT, JAVASCRIPT) = true
      | eq (BYTECODE, BYTECODE) = true
      | eq (SAY, SAY) = true
      | eq (PUT, PUT) = true
      | eq (INLINE, INLINE) = true
      | eq (HOLD, HOLD) = true
      | eq (LETA, LETA) = true
      | eq (LETSHAM, LETSHAM) = true
      | eq (SHAM, SHAM) = true
      | eq _ = false
end