/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */

%option noyywrap


%{
#include "cool-parse.h"
#include "stringtab.h"
#include "utilities.h"

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int comment_level = 0;			// Nível de comentários aninhados.
std::string input = "";			// String de entrada. Usar string_buf não tá dando...
bool invalid_string = false;	// Indica se há caracteres inválidos na string.
%}
/* ----------------------------------------------------------------- */
/* -------------------------- DEFINITIONS -------------------------- */
/* ----------------------------------------------------------------- */

/* 'int lineno() const' -> returns the current input line number [...].*/
%option yylineno

/*
 * Define names for regular expressions here.
 */

DARROW					=>
ASSIGN					<-
DIGIT					[0-9]
CHAR					[A-Za-z]
UPPER-CASE				[A-Z]
LOWER-CASE				[a-z]
ANY_CHAR				.
DOUBLE_QUOTE			\"
INTEGER					{DIGIT}+
TYPEID					{UPPER-CASE}({CHAR}|{DIGIT}|"_")*
OBJECTID				{LOWER-CASE}({CHAR}|{DIGIT}|"_")*
LINE_COMMENT			("--")
COMMENT_INIT			("(*")
COMMENT_END				("*)")
WHITESPACES				(" "|"\n"|"\f"|"\r"|"\t"|"\v")
/* SPECIAL_NOTATION		("Object"|"Int"|"Bool"|"String"|"SELF_TYPE"|"self")*/
/* Special notation must be treated as any other identifier. */

%x COMMENT STRING

%%
 /* ----------------------------------------------------------------- */
 /* ----------------------------- RULES ----------------------------- */
 /* ----------------------------------------------------------------- */

 /*
  *  Line comments.
  */

{LINE_COMMENT}(.)*					{ }

 /*
  *  Nested comments
  */

<INITIAL,COMMENT>{COMMENT_INIT}		{ comment_level++; BEGIN(COMMENT); }

<INITIAL,COMMENT>\n					{ curr_lineno = yylineno; }

<INITIAL>{COMMENT_END}				{
	cool_yylval.error_msg = "Unmatched *)";
	return ERROR;
}

<COMMENT>{COMMENT_END}				{
	comment_level--;
	if(comment_level == 0)
	{
		input = "";
		BEGIN(INITIAL);
	}
}

<COMMENT><<EOF>>					{
	BEGIN(INITIAL);
	cool_yylval.error_msg = "EOF in comment";
	return ERROR;
}

<COMMENT>{ANY_CHAR}					{ input += yytext; }

 /*
  *  The character operators.
  */
{DARROW}							{ return DARROW; }
{ASSIGN}							{ return ASSIGN; }
"<="								{ return LE; }
"+"									{ return '+'; }
"-"									{ return '-'; }
"*"									{ return '*'; }
"/"									{ return '/'; }
"<"									{ return '<'; }
"="									{ return '='; }
"~"									{ return '~'; }
"."									{ return '.'; }
","									{ return ','; }
":"									{ return ':'; }
";"									{ return ';'; }
"@"									{ return '@'; }
"("									{ return '('; }
")"									{ return ')'; }
"{"									{ return '{'; }
"}"									{ return '}'; }

 /*
  *  Keywords are case-insensitive except for the values true and false,
  *  which must begin with a lower-case letter.
  */
("C"|"c")("L"|"l")("A"|"a")("S"|"s")("S"|"s")								{ return CLASS; }
("E"|"e")("L"|"l")("S"|"s")("E"|"e")										{ return ELSE; }
("F"|"f")("I"|"i")															{ return FI; }
("I"|"i")("F"|"f")															{ return IF; }
("I"|"i")("N"|"n")															{ return IN; }
("I"|"i")("N"|"n")("H"|"h")("E"|"e")("R"|"r")("I"|"i")("T"|"t")("S"|"s")	{ return INHERITS; }
("I"|"i")("S"|"s")("V"|"v")("O"|"o")("I"|"i")("D"|"d")						{ return ISVOID; }
("L"|"l")("E"|"e")("T"|"t")													{ return LET; }
("L"|"l")("O"|"o")("O"|"o")("P"|"p")										{ return LOOP; }
("P"|"p")("O"|"o")("O"|"o")("L"|"l")										{ return POOL; }
("T"|"t")("H"|"h")("E"|"e")("N"|"n")										{ return THEN; }
("W"|"w")("H"|"h")("I"|"i")("L"|"l")("E"|"e")								{ return WHILE; }
("C"|"c")("A"|"a")("S"|"s")("E"|"e")										{ return CASE; }
("E"|"e")("S"|"s")("A"|"a")("C"|"c")										{ return ESAC; }
("N"|"n")("E"|"e")("W"|"w")													{ return NEW; }
("O"|"o")("F"|"f")															{ return OF; }
("N"|"n")("O"|"o")("T"|"t")													{ return NOT; }
"t"("R"|"r")("U"|"u")("E"|"e")												{ cool_yylval.boolean = true; return BOOL_CONST; }
"f"("A"|"a")("L"|"l")("S"|"s")("E"|"e")										{ cool_yylval.boolean = false; return BOOL_CONST; }

 /*
  *  Identifiers and integers.
  */
{OBJECTID}		{
	cool_yylval.symbol = idtable.add_string(yytext, yyleng);
	return OBJECTID;
}

{TYPEID}		{
	cool_yylval.symbol = idtable.add_string(yytext, yyleng);
	return TYPEID;
}

{INTEGER}		{
	cool_yylval.symbol = inttable.add_string(yytext, yyleng);
	return INT_CONST;
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  */
<STRING>\\\0			{
	invalid_string = true;
	BEGIN(INITIAL);
	cool_yylval.error_msg = "String contains escaped null character.";
	return ERROR;
}

<STRING>(\\.|\\\n)		{
	char converter_escaped = yytext[1];
	if(yytext[1] == 'n') converter_escaped = '\n';
	else if(yytext[1] == 't') converter_escaped = '\t';
	else if(yytext[1] == 'b') converter_escaped = '\b';
	else if(yytext[1] == 'f') converter_escaped = '\f';
	input += converter_escaped;
}

<STRING>{DOUBLE_QUOTE}	{
	BEGIN(INITIAL);
	if(input.length() > MAX_STR_CONST)
	{
		cool_yylval.error_msg = "String constant too long";
		input = "";
		return ERROR;
	}
	else if(!invalid_string)
	{
		cool_yylval.symbol = stringtable.add_string((char *) input.c_str());
		input = "";
		return STR_CONST;
	}
}

<STRING><<EOF>>			{
	BEGIN(INITIAL);
	invalid_string = true;
	cool_yylval.error_msg = "EOF in string constant";
	return ERROR;
}

<STRING>\0			{
	invalid_string = true;
	cool_yylval.error_msg = "String contains null character";
	return ERROR;
}

<STRING>\n				{
	curr_lineno = yylineno;
	cool_yylval.error_msg = "Unterminated string constant";
	BEGIN(INITIAL);
	return ERROR;
}

<STRING>{ANY_CHAR}		{ input += yytext; }

{DOUBLE_QUOTE}			{
	invalid_string = false;
	BEGIN(STRING);
	curr_lineno = yylineno;
}

{WHITESPACES}			{ }

{ANY_CHAR}				{ cool_yylval.error_msg = yytext; return ERROR; }

%%