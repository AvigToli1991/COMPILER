/*scanner-file*/
%{
#include<stdio.h>
#include "y.tab.h"
%}

%%

bool {return BOOL;}
char {return CHAR;}
int {return INT;}
real {return REAL;}
string {return STRING;}
int\* {return INTPTR;}
char\* {return CHARPTR;}
real\* {return REALPTR;}

if {return IF;}
else {return ELSE;}
while {return WHILE;}
for {return FOR;}

var {return VAR;}
arg-> {return ARG;}

function {return FUNC;}
return {return RETURN;}
null {return NULL;}
void {return VOID;}
do {return DO}

"&&" {return AND;}
"/" return yytext[0];
"=" return yytext[0];
"==" {return EQ;}
">" return yytext[0];
">=" {return GTE;}
"<=" {return LTE;}
"<" return yytext[0];
"-" return yytext[0];
"!" {return NOT;}
"!=" {return NOTEQ;}
"|" {return LENGTH;}
"||" {return OR;}
"+" return yytext[0];
"*" return yytext[0];
"&" return yytext[0];
";" return yytext[0];
"," return yytext[0];
"{" return yytext[0];
"}" return yytext[0];
"(" return yytext[0];
")" return yytext[0];
"[" return yytext[0];
"]" return yytext[0];

"/""*".*"*""/" {return COMMENT;}

true|false {return BOOLVAL;}
[a-zA-Z][a-zA-Z0-9_]* { yylval.string= strdup(yytext);return ID;}
"\""[^"]*"\"" {yylval.string= strdup(yytext);return STR_VAL;}
\'.\' {yylval.string= strdup(yytext);return CHAR_VAL;}
[+-]?[1-9][0-9]*|[0-9] {yylval.string= strdup(yytext);return DEC_VAL;}
0(x|X)[0-9A-F]+ {yylval.string= strdup(yytext);return HEX_VAL;}
[-+]?[0-9]*\.?[0-9]*[eE]?[-+]?[0-9]+ {yylval.string= strdup(yytext);return REAL_VAL;}

[\n] {yylineno++;} //For each line (enter=\n), do yylineo+=1
[ \t]+ ;
. return yytext[0];
%%
int yywrap(void)        /* called at end of input */
{
    return 1;           /* terminate now */
}

