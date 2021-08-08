%{

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#pragma warning(disable : 4996)

FILE* yyin;
extern int yylineno;
void yyerror();

extern int yylex();

int number_string = 1;

%}

%start commands


%token NUMBER ATOM_STRING VAR
%token DOT COMMA SLASH SEMICOLON FORWARD_SLASH BACKSLASH
%token RULES INEQUALITY
%token DYNAMIC USE_MODULE LIBRARY AS EXCEPT EXISTS_SOURCE MODULE IS
%token IF ELIF ELSE ENDIF IFTHEN
%token OPENBRACKET CLOSEBRACKET OPENBRACKET_SQUARE CLOSEBRACKET_SQUARE OPENBRACKET_CURLE CLOSEBRACKET_CURLE
%token PLUS MINUS MUL OPERATION
%token STRING
%token EXCLAMATION_MARK

%%
commands: 
	command
	| 
	commands command
	;

command:
	PLUS command
	|
	MINUS command
	|
	RULES USE_MODULE OPENBRACKET lib CLOSEBRACKET DOT
	|
	RULES DYNAMIC dyn_parametrs DOT
	|
	RULES MODULE OPENBRACKET ATOM_STRING CLOSEBRACKET DOT
	|
	if_start if_body if_else else_body if_end
	|
	if_start if_body if_end
	|
	ATOM_STRING OPENBRACKET rules_parametrs CLOSEBRACKET DOT
	|
	ATOM_STRING OPENBRACKET rules_parametrs CLOSEBRACKET RULES rules_body DOT
	|
	ATOM_STRING RULES rules_body DOT
	|
	VAR RULES rules_body DOT
	;

expression:
	EXISTS_SOURCE OPENBRACKET LIBRARY OPENBRACKET lib_name CLOSEBRACKET CLOSEBRACKET
	|
	is_parametrs INEQUALITY is_parametrs
	|
	ATOM_STRING OPENBRACKET rules_parametrs CLOSEBRACKET
	;


/* START RULES */
rules_body:
	rule
	|
	rules_body COMMA rule
	|
	rules_body SEMICOLON rule
	;

rule:
	ATOM_STRING rule_atom_string
	|
	OPENBRACKET	expression IFTHEN ifthen_body SEMICOLON ifthen_body CLOSEBRACKET
	|
	OPENBRACKET	rules_body CLOSEBRACKET
	|
	OPENBRACKET_CURLE rules_body CLOSEBRACKET_CURLE
	|
	expression
	|
	VAR IS is_body
	|
	EXCLAMATION_MARK
	|
	OPENBRACKET_SQUARE list CLOSEBRACKET_SQUARE
	|
	is_body
	|
	PLUS rule
	|
	MINUS rule
	|
	BACKSLASH PLUS rule
	|
	BACKSLASH MINUS rule
	;

rule_atom_string:
	|
	OPENBRACKET rules_parametrs CLOSEBRACKET
	;

rules_parametrs:
	var_parametrs
	|
	rules_parametrs COMMA var_parametrs
	|
	rules_parametrs SEMICOLON var_parametrs
	;
	
var_parametrs:
	ATOM_STRING SLASH NUMBER
	|
	expression
	|
	ATOM_STRING
	|
	NUMBER
	|
	VAR IS is_body
	|
	VAR
	|
	is_body
	|
	OPENBRACKET_SQUARE list CLOSEBRACKET_SQUARE
	|
	STRING MINUS var_parametrs
	|
	STRING
	|
	PLUS var_parametrs
	|
	MINUS var_parametrs
	|
	OPENBRACKET rules_parametrs CLOSEBRACKET
	;
/* END RULES */


/* START BLOCK FOR IS */
is_body:
	is_body OPERATION is_parametrs
	|
	is_body MINUS is_parametrs
	|
	is_body PLUS is_parametrs
	|
	is_body	MUL is_parametrs
	|
	is_body	SLASH is_parametrs
	|
	is_parametrs
	;

is_parametrs:
	NUMBER
	|
	ATOM_STRING
	|
	VAR
	|
	OPENBRACKET is_parametrs CLOSEBRACKET
	|
	OPENBRACKET_SQUARE list CLOSEBRACKET_SQUARE
	|
	ATOM_STRING OPENBRACKET rules_parametrs CLOSEBRACKET
	;
/* END BLOCK FOR IS*/


/* START BLOCK FOR LOAD LIBRARY (use_module) */
lib:
	LIBRARY OPENBRACKET lib_name CLOSEBRACKET lib_end
	|
	lib_name lib_end
	;

lib_name:
	ATOM_STRING
	| 
		lib_name SLASH ATOM_STRING
	;

lib_end:
	|
	COMMA OPENBRACKET_SQUARE lib_options CLOSEBRACKET_SQUARE
	|
	COMMA EXCEPT OPENBRACKET OPENBRACKET_SQUARE except_options CLOSEBRACKET_SQUARE CLOSEBRACKET
	;

lib_options:
	|
	ATOM_STRING SLASH NUMBER lib_end_options
	|
	lib_options COMMA ATOM_STRING SLASH NUMBER lib_end_options
	;

lib_end_options:
	|
	AS ATOM_STRING
	;

except_options:
	ATOM_STRING SLASH NUMBER
	|
	except_options COMMA ATOM_STRING SLASH NUMBER
	;
/* END BLOCK FOR LOAD LIBRARY (use_module) */


/* START BLOCK FOR IF */
if_start:
	RULES IF OPENBRACKET expression CLOSEBRACKET DOT
	;

if_else:
	RULES ELSE DOT
	;

if_end:
	RULES ENDIF DOT
	;

if_body:
	else_body
	|
	if_body RULES ELIF OPENBRACKET expression CLOSEBRACKET DOT command
	;

else_body:
	|
	command
	|
	if_body command
	;
/* END BLOCK FOR IF */


/* START DYNAMIC PARAMETRS*/
dyn_parametrs:
	ATOM_STRING SLASH NUMBER
	|
	dyn_parametrs COMMA ATOM_STRING SLASH NUMBER
	;
/* END DYNAMIC PARAMETRS*/


/* START IFTHEN*/
ifthen_body:
	rule
	|
	ifthen_body COMMA rule
	;
/* END IFTHEN*/


/* START LIST BLOCK*/
list:
	|
	var_parametrs FORWARD_SLASH var_parametrs
	|
	list_parametrs
	;

list_parametrs:
	var_parametrs
	|
	list_parametrs COMMA var_parametrs
	;
/* END LIST BLOCK*/
%%

int parser_main(int argc, char* argv[])
{
	if (argc > 2)
	{
		printf("Error: too many parameters\n");
		return 1;
	}
	if (argc == 2)
	{
		yyin = fopen(argv[1], "rb");
		if (yyin == NULL)
		{
			printf("Error: file wasn't opened\n");
			return 1;
		}
		yyparse();
		fclose(yyin);
	}
	return 0;
}

void yyerror(const char* error_string)
{
	printf("Error: %s (%d)\n", error_string, number_string);
	exit(1);
}