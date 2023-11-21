%{
#include <stdio.h>
#include "symantic.hpp"
#include "parser.tab.hpp"
%}

%option yylineno
%option noyywrap
digit   		([0-9])
digit_no_zero  	([1-9])
letter  		([a-zA-Z])
whitespace		([\t \n\r])
hexadecimal     (\\x[0-7][0-9A-Fa-f])
legal_escape    (\\[\\"nrt0])
ilegal_escape   (\\[^\\"nrt0])
printable       ([ !#-\[\]-~])


%%
void            yylval=new Node(yytext);            return VOID;
int             yylval=new Node(yytext);            return INT;
byte            yylval=new Node(yytext);            return BYTE;
b               yylval=new Node(yytext);            return B;
bool            yylval=new Node(yytext);            return BOOL;
override        yylval=new Node(yytext);            return OVERRIDE;
and             yylval=new Node(yytext);            return AND;
or              yylval=new Node(yytext);            return OR;
not             yylval=new Node(yytext);            return NOT;
true            yylval=new Node(yytext);            return TRUE;
false           yylval=new Node(yytext);            return FALSE;
return          yylval=new Node(yytext);            return RETURN;
if              yylval=new Node(yytext);            return IF;
else            yylval=new Node(yytext);            return ELSE;
while           yylval=new Node(yytext);            return WHILE;
break           yylval=new Node(yytext);            return BREAK;
continue        yylval=new Node(yytext);            return CONTINUE;
(\;)            yylval=new Node(yytext);            return SC;
(\,)            yylval=new Node(yytext);            return COMMA;
\(              yylval=new Node(yytext);            return LPAREN;
\)              yylval=new Node(yytext);            return RPAREN;
\{              yylval=new Node(yytext);            return LBRACE;
\}              yylval=new Node(yytext);            return RBRACE;
(\=)            yylval=new Node(yytext);            return ASSIGN;
(==|!=|<|>|<=|>=) yylval=new Node(yytext);          return RELOP;
(\+|\-)         yylval=new Node(yytext);            return BINOP_MULT;
(\*|\/)         yylval=new Node(yytext);            return BINOP_PLUS;
\/\/([^\n\r]*)  yylval=new Node(yytext);            return COMMENT;
{letter}+({digit}|{letter})* yylval=new Node(yytext);   return ID;
({digit_no_zero}+{digit}*)|0 yylval=new Node(yytext);   return NUM;
\"([^\n\r\"\\]|\\[rnt"\\])+\" yylval=new Node(yytext); return STRING;
{whitespace} ;
\/\/[^\r\n]*[\r|\n|\r\n]? ;
. {output::errorLex(yylineno);exit(0);}
%%