grammar XDR;

@parser::header {
    package edu.vtc.xdr2os3;
}

@lexer::header {
    package edu.vtc.xdr2os3;
}

@members {

// The following material was from an earlier ANTLR3 grammar. It may not be applicable with
// ANTLR4. Aborting after the first syntax error is not acceptable in a production parser
// anyway. However, I'm keeping this material here for now in case it proves to be a useful
// reference.

//    // The following two magic methods, together with the @rulecatch section below cause the
//    // parser to exit immediately with an exception when an error is encountered.
//    //
//    protected Object recoverFromMismatchedToken(IntStream input, int ttype, BitSet follow)
//        throws RecognitionException
//    {
//        throw new MismatchedTokenException(ttype, input);
//    }
//
//    public Object recoverFromMismatchedSet(IntStream input, RecognitionException e, BitSet follow)
//        throws RecognitionException
//    {
//        throw e;
//    }
}

//@parser::rulecatch {
//    catch (RecognitionException e) {
//        throw e;
//    }
//}

/* ======= */
/* Grammar */
/* ======= */

// The productions below are from the XDR standard (RFC-4506). They have been augmented with
// facilities supporting the definition of types with range constraints.

declaration:
      type_specifier IDENTIFIER
    | type_specifier IDENTIFIER EQUALS CONSTANT
    | type_specifier IDENTIFIER LBRACKET value RBRACKET
    | type_specifier IDENTIFIER LANGLE value? RANGLE
    | OPAQUE IDENTIFIER LBRACKET value RBRACKET
    | OPAQUE IDENTIFIER LANGLE value? RANGLE
    | STRING IDENTIFIER LANGLE value? RANGLE
    | IDENTIFIER LANGLE value? RANGLE
    | type_specifier STAR IDENTIFIER
    | VOID;

subtype_spec:
      IS IDENTIFIER
    | IS NATURAL;

value:
    CONSTANT | IDENTIFIER;

type_specifier:
      UNSIGNED? INT
    | UNSIGNED? HYPER
    | FLOAT
    | DOUBLE
    | QUADRUPLE
    | BOOL
    | STRING
    | IDENTIFIER
    | TIME
    | TIME_SPAN
    | DATA
    | enum_type_spec
    | struct_type_spec
    | union_type_spec;

enum_type_spec:
    ENUM enum_body;

enum_body:
    LBRACE IDENTIFIER EQUALS value (COMMA IDENTIFIER EQUALS value)* RBRACE
    | LBRACE IDENTIFIER (COMMA IDENTIFIER)* RBRACE;

struct_type_spec:
    STRUCT struct_body;

struct_body:
    LBRACE (declaration SEMI)+ RBRACE;

union_type_spec:
    UNION union_body;

union_body:
    SWITCH LPARENS declaration RPARENS LBRACE
      case_spec+
      (DEFAULT COLON declaration SEMI)?
    RBRACE;

case_spec:
    (CASE value COLON)+ declaration SEMI;

constant_def:
      CONST IDENTIFIER EQUALS CONSTANT SEMI
    | CONST IDENTIFIER IS IDENTIFIER EQUALS CONSTANT SEMI;

type_def:
      TYPEDEF declaration RANGE range_constraint SEMI
    | TYPEDEF declaration subtype_spec RANGE range_constraint SEMI
    | TYPEDEF declaration SEMI
    | ENUM IDENTIFIER enum_body SEMI
    | STRUCT IDENTIFIER struct_body SEMI
    | UNION IDENTIFIER union_body SEMI
    | MESSAGE STRUCT (LARROW | RARROW) IDENTIFIER struct_body (condition)? SEMI;

range_constraint:
      CONSTANT DOTDOT CONSTANT
    | CONSTANT DOTDOT IDENTIFIER;

line:
     declaration SEMI;

definition:
    type_def | constant_def | encoder_def | decoder_def | line;

specification:
    definition*;

// The declarations below are extensions for CubedOS.

encoder_def:
    ENCODER IDENTIFIER LPARENS declaration (COMMA declaration)* RPARENS SEMI;

decoder_def:
    DECODER IDENTIFIER LPARENS declaration (COMMA declaration)* RPARENS SEMI;

/* =========== */
/* Lexer rules */
/* =========== */

// --------------
// Reserved Words
// --------------
BOOL      : 'bool';
CASE      : 'case';
CONST     : 'const';
DECODER   : 'decoder';
DEFAULT   : 'default';
DOUBLE    : 'double';
ENCODER   : 'encoder';
ENUM      : 'enum';
FLOAT     : 'float';
HYPER     : 'hyper';
INT       : 'int';
OPAQUE    : 'opaque';
QUADRUPLE : 'quadruple';
RANGE     : 'range';
STRING    : 'string';
STRUCT    : 'struct';
SWITCH    : 'switch';
TYPEDEF   : 'typedef';
UNION     : 'union';
UNSIGNED  : 'unsigned';
VOID      : 'void';
MESSAGE   : 'message';
IS        : 'is';
NATURAL   : 'Natural';
TIME      : 'Ada.Real_Time.Time';
TIME_SPAN : 'Ada.Real_Time.Time_Span';
DATA      : 'CubedOS.Lib.Octet_Array';
WITH      : 'with';
M_I       : 'message_invariant';

// -------
// Symbols
// -------
COLON    : ':';
COMMA    : ',';
DOTDOT   : '..';
LANGLE   : '<';
LBRACE   : '{';
LBRACKET : '[';
LPARENS  : '(';
EQUALS   : '=';
RANGLE   : '>';
RBRACE   : '}';
RBRACKET : ']';
RPARENS  : ')';
SEMI     : ';';
STAR     : '*';
LARROW   : '<-';
RARROW   : '->';
RPOINT   : '=>';
LOE      : '<=';
GOE      : '>=';

IDENTIFIER
    :   [a-zA-Z][a-zA-Z0-9_']*;

condition
    :   (WITH M_I RPOINT expression COMMA)* WITH M_I RPOINT expression;

expression
    :   IDENTIFIER LOE IDENTIFIER
    |   IDENTIFIER GOE IDENTIFIER
    |   IDENTIFIER RANGLE IDENTIFIER
    |   IDENTIFIER LANGLE IDENTIFIER
    |   IDENTIFIER EQUALS IDENTIFIER;

WHITESPACE
    :   [ \t\f\r\n]+  -> skip;

COMMENT1
    :    '/*' .*? '*/' -> skip;

COMMENT2
    :   '//' .*? [\r\n] -> skip;

CONSTANT
    :   ('-')? (DECIMAL | BASED) ( ('.') DIGIT+ )? ( ('E' | 'e') DIGIT+ )?;

fragment DECIMAL
    :   DIGIT ('_'? DIGIT)*;

fragment BASED
    :   DIGIT+ '#' HDIGIT ('_'? HDIGIT)* '#';

fragment DIGIT
    :   [0-9];

fragment HDIGIT
    :   [0-9a-fA-F];
