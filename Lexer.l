%{

#include "Parser.tab.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define fileno _fileno

extern int number_string;

%}

%option noyywrap
%option yylineno
%x long_comment
%x string_1
%x string_2

%%

[0-9]+					{ printf("number"); return NUMBER; }
[0-9]+\.[0-9]+			{ printf("float_number"); return NUMBER; }

:-						{ printf(":-"); return RULES; }
-->						{ printf("-->"); return RULES; }

\\==					{ printf("\\=="); return INEQUALITY; }
\\=						{ printf("\\="); return INEQUALITY; }
=\\=					{ printf("=\\="); return INEQUALITY; }
=\:=					{ printf("=:="); return INEQUALITY; }
\\=@=					{ printf("\\=@="); return INEQUALITY; }
=@=						{ printf("=@="); return INEQUALITY; }
==						{ printf("=="); return INEQUALITY; }
=\.\.					{ printf("=.."); return INEQUALITY; }
@>=						{ printf("@>="); return INEQUALITY; }
@>						{ printf("@>"); return INEQUALITY; }
@=\<					{ printf("@=<"); return INEQUALITY; }
@<						{ printf("@>"); return INEQUALITY; }
>=						{ printf(">="); return INEQUALITY; }
=\<						{ printf("=<"); return INEQUALITY; }
>						{ printf(">"); return INEQUALITY; }
\<						{ printf("<"); return INEQUALITY; }
=						{ printf("="); return INEQUALITY; }

div						{ printf("div"); return OPERATION; }
rdiv					{ printf("rdiv"); return OPERATION; }
mod						{ printf("mod"); return OPERATION; }
rem						{ printf("rem"); return OPERATION; }
xor						{ printf("xor"); return OPERATION; }
gcd						{ printf("gcd"); return OPERATION; }
lcm						{ printf("lcm"); return OPERATION; }
\/\\					{ printf("\/\\"); return OPERATION; }
\\\/					{ printf("\\/"); return OPERATION; }
\/\/					{ printf("//"); return OPERATION; }
\>\>					{ printf(">>"); return OPERATION; }
\<\<					{ printf("<<"); return OPERATION; }
\*\*					{ printf("**"); return OPERATION; }
\^						{ printf("^"); return OPERATION; }

\+						{ printf("+"); return PLUS; }
\-						{ printf("-"); return MINUS; }
\*						{ printf("*"); return MUL; }

\!						{ printf("!"); return EXCLAMATION_MARK; }

dynamic[ \r\n]			{ printf("dynamic "); return DYNAMIC; }
use_module				{ printf("use_module"); return USE_MODULE; }
library					{ printf("library"); return LIBRARY; }
as[ \r\n]				{ printf("as "); return AS; }
except					{ printf("except"); return EXCEPT; }
exists_source			{ printf("exists_source"); return EXISTS_SOURCE; }
module					{ printf("module"); return MODULE; }
is[ \r\n]				{ printf("is "); return IS; }

if						{ printf("if"); return IF; }
elif					{ printf("elif"); return ELIF; }
else					{ printf("else"); return ELSE; }
endif					{ printf("endif"); return ENDIF; }

->						{ printf("->"); return IFTHEN; }


[\n]+					{number_string++; printf("\n"); }
[\t]+					{ printf("\t"); }

[a-z][A-Za-z0-9_]*		{ printf("atom_string "); return  ATOM_STRING; }
[A-Z_][A-Za-z0-9_]*		{ printf("var "); return VAR; }

\/						{ printf("\/"); return SLASH; }
\\						{ printf("\\"); return BACKSLASH; }
\.						{ printf("."); return DOT; }
\,						{ printf(","); return COMMA; }
\;						{ printf(";"); return SEMICOLON; }
\|						{ printf("|"); return FORWARD_SLASH; }

\(						{ printf("("); return OPENBRACKET; }
\)						{ printf(")"); return CLOSEBRACKET; }
\[						{ printf("["); return OPENBRACKET_SQUARE; }
\]						{ printf("]"); return CLOSEBRACKET_SQUARE;}
\[						{ printf("{"); return OPENBRACKET_CURLE; }
\]						{ printf("}"); return CLOSEBRACKET_CURLE;}


\%[^\n]*				{ printf("%% comment:)"); }
\/\*					BEGIN(long_comment);
\'						BEGIN(string_1);
\"						BEGIN(string_2);

<long_comment>
{
	"*/"		BEGIN(0); printf("/* comment */");
	\n			number_string++;
	.			;
}

<string_1>
{
	\\\'		;
	"\'"		BEGIN(0); printf("\'string\'"); return STRING;
	.			;
}

<string_2>
{
	\\\"		;
	"\""		BEGIN(0); printf("\"string\""); return STRING;
	.			;	
}


%%