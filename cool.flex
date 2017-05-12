	/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>
#include <iostream>
#include <string>

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

#define RET_ERROR(s) {\
	cool_yylval.error_msg = (s);\
	return ERROR;}

#define ADD_CHAR_TO_STR(c) \
	if ( string_buf_ptr - string_buf + 1 < MAX_STR_CONST ) \
		*string_buf_ptr++ = (c);\
	else {\
		BEGIN(skip);\
		RET_ERROR("String constant too long")\
	}

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

int num_comment_depth = 0;

/*
 *  Add Your own definitions here
 */

%}

/*
 * Define names for regular expressions here.
 */

CLASS		(C|c)(L|l)(A|a)(S|s)(S|s)
ELSE		(E|e)(L|l)(S|s)(E|e)
FI		(F|f)(I|i)
IF		(I|i)(F|f)
IN		(I|i)(N|n)
INHERITS	(I|i)(N|n)(H|h)(E|e)(R|r)(I|i)(T|t)(S|s)
LET		(L|l)(E|e)(T|t)
LOOP		(L|l)(O|o)(O|o)(P|p)
POOL		(P|p)(O|o)(O|o)(L|l)
THEN		(T|t)(H|h)(E|e)(N|n)
WHILE		(W|w)(H|h)(I|i)(L|l)(E|e)
CASE		(C|c)(A|a)(S|s)(E|e)
ESAC		(E|e)(S|s)(A|a)(C|c)
OF		(O|o)(F|f)
NEW		(N|n)(E|e)(W|w)
ISVOID		(I|i)(S|s)(V|v)(O|o)(I|i)(D|d)
NOT		(N|n)(O|o)(T|t)

DARROW =>
ASSIGN <-
LE <=

TRUE t[rR][uU][eE]
FALSE f[aA][lL][sS][eE]

INT_CONST	[0-9]+
OBJECTID	[a-z][A-Za-z0-9_]*
TYPEID		[A-Z][A-Za-z0-9_]*

SINGLE		"+"|"-"|"*"|"/"|"="|"<"|"."|"~"|","|";"|":"|"("|")"|"@"|"{"|"}"
WHITE		" "|\t|\f|\r|\v

ANY_CHAR	.

COMM_LINE	"--"[^\n\0]*
COMM_OPEN	"(*"
COMM_CLOSE	"*)"

%Start		str_cond skip comm 




%%



 /* Do nothing with one line comment
  * But only in the INITIAL state
 */
<INITIAL>{COMM_LINE}		;
 /* Increase comment depth when in comments or in INITIAL state */
<INITIAL,comm>{COMM_OPEN}	{
	BEGIN(comm);
	num_comment_depth++;
}
<INITIAL>{COMM_CLOSE}		RET_ERROR("Unmatched *)")
 /* Comment could be closed only when it is started */
<comm>{COMM_CLOSE}			{
	num_comment_depth--;
	if ( num_comment_depth == 0 ) {
		BEGIN(INITIAL);
	}
}
 /* Show error when meet EOF in comments */
<comm><<EOF>>				{
	BEGIN(INITIAL);
	RET_ERROR("EOF in comment")
}
<comm>\n					curr_lineno++;
 /* Do nothing for other characters */
<comm>.						;


<INITIAL>\" {
	string_buf_ptr = string_buf;
	string_buf_ptr[0] = 0;
	BEGIN(str_cond);
}

<str_cond><<EOF>> 	{
	BEGIN(INITIAL);
	RET_ERROR("EOF in string constant")
}

 /* For escaped new line, regard it as new line character */
<str_cond>\\\n		{
	curr_lineno++;
	ADD_CHAR_TO_STR('\n');
}

 /* For single new line, show error and then auto come to next line */
 /* But notice that the state should be turned into INITIAL */
<str_cond>\n			{
	curr_lineno++;
	BEGIN(INITIAL);
	RET_ERROR("Unterminated string constant")
}

 /* For null character, manually skip to end of the string */
<str_cond>\0			{
	BEGIN(skip);
	RET_ERROR("String contains null character")
}
<str_cond>\\\0			{
	BEGIN(skip);
	RET_ERROR("String contains escaped null character")
}

<str_cond>\\b	{ ADD_CHAR_TO_STR('\b');}
<str_cond>\\t	{ ADD_CHAR_TO_STR('\t');}
<str_cond>\\n	{ ADD_CHAR_TO_STR('\n');}
<str_cond>\\f	{ ADD_CHAR_TO_STR('\f');}
<str_cond>\\.	{ ADD_CHAR_TO_STR(yytext[1]);}

<str_cond>\" {
	*string_buf_ptr = 0;
	cool_yylval.symbol = stringtable.add_string(strdup(string_buf));
	//std::cout << "string added = " << string_buf << "\n";
	BEGIN(INITIAL);
	return STR_CONST;
}

<str_cond>. 	{ ADD_CHAR_TO_STR(yytext[0]); }

 /* Skip mode: ignore any character until quote or newline */
<skip>\n		{curr_lineno++; BEGIN(INITIAL);}
<skip>\"		BEGIN(INITIAL);
<skip>.			;

<INITIAL>{CLASS} { return CLASS; }
<INITIAL>{IF} { return IF; }
<INITIAL>{ELSE} { return ELSE; }
<INITIAL>{THEN} { return THEN; }
<INITIAL>{FI} { return FI; }
<INITIAL>{IN} { return IN; }
<INITIAL>{INHERITS} { return INHERITS; }
<INITIAL>{LET} { return LET; }
<INITIAL>{CASE} { return CASE; }
<INITIAL>{ESAC} { return ESAC; }
<INITIAL>{POOL} { return POOL; }
<INITIAL>{LOOP} { return LOOP; }
<INITIAL>{WHILE} { return WHILE; }
<INITIAL>{OF} { return OF; }
<INITIAL>{NOT} { return NOT; }
<INITIAL>{NEW} { return NEW; }
<INITIAL>{ISVOID} { return ISVOID; }

<INITIAL>{INT_CONST} {
	cool_yylval.symbol = inttable.add_string(yytext);
	//std::cout << "int_const = " << yytext << "\n";
	return INT_CONST;
}

<INITIAL>{TRUE}|{FALSE} {
	cool_yylval.boolean = std::string(yytext) == "true";
	//std::cout << "bool_const = " << yytext << "\n";
	return BOOL_CONST;
}

<INITIAL>{OBJECTID}|(self) {
	cool_yylval.symbol = idtable.add_string(yytext);
	//std::cout << "identifier = " << yytext << "\n";
	return OBJECTID;
}

<INITIAL>{TYPEID}|(SELF_TYPE) {
	cool_yylval.symbol = idtable.add_string(yytext);
        //std::cout << "type = " << yytext << "\n";
	return TYPEID;
}

 /*
  *  Nested comments
  */


 /*
  *  The multiple-character operators.
  */

<INITIAL>{DARROW} { return DARROW; }
<INITIAL>{ASSIGN} { return ASSIGN; }
<INITIAL>{LE} { return LE; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

<INITIAL>{SINGLE}		{ return yytext[0]; }
<INITIAL>{WHITE}		;
<INITIAL>\n			{ curr_lineno++; }

<INITIAL>{ANY_CHAR} 		RET_ERROR(yytext);


%%
