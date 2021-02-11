%{ 
	#include <stdio.h>
	#include <stdlib.h>
	#include "y.tab.h"

	#define YY_DECL int yylex()

	FILE* output;
	int quotationCounter = 0;
%} 

%option caseless

NON_ZERO_DIGIT			[1-9]
DIGIT 					[0-9]*
IDENTIFIER	    		[a-z][a-z0-9]*

%%
0						{yylval.type.integer = atoi(yytext); return VALUE;}
0*						{return ERROR;}
{NON_ZERO_DIGIT}{DIGIT}	{yylval.type.integer = atoi(yytext); return VALUE;}
";;"[^\n]*""			/*return COMMENT;*/
and						return KW_AND;								
or 						return KW_OR;								
not						return KW_NOT;								
equal					return KW_EQUAL;							
less					return KW_LESS;							
nil						return KW_NIL;							
list					return KW_LIST;							
append					return KW_APPEND;						
concat					return KW_CONCAT;						
set						return KW_SET;							
deffun					return KW_DEFFUN;						
for						return KW_FOR;							
if						return KW_IF;							
exit					return KW_EXIT;							
load					return KW_LOAD;							
disp					return KW_DISP;							
true					return KW_TRUE;							
false					return KW_FALSE;						
"+"						return OP_PLUS;							
"-"						return OP_MINUS;						
"/"						return OP_DIV;							
"*"						return OP_MULT;							
"("						return OP_OP;							
")"						return OP_CP;							
"**"					return OP_DBLMULT;						
"“"						return OP_OC;							
"”"						return OP_CC;							
"\""					quotationCounter++; if(quotationCounter % 2 == 1) {printf("OP_OC\n"); fprintf(output, "OP_OC\n"); return OP_OC;} else {printf("OP_CC\n"); fprintf(output, "OP_CC\n"); return OP_CC;}	
","						return OP_COMMA;								
" "
\n						
\r						
\t
{IDENTIFIER}			{yylval.type.name = strdup(yytext); return IDENTIFIER;}  							
{IDENTIFIER}+[^{IDENTIFIER}" "\n\r\t"("")""\""]+ printf("ERROR the  d symbol is not an identifier\n");	fprintf(output, "ERROR the symbol is not an identifier\n"); return -1;		
\n\n	 				return 0;
[0-9]+[a-z]*			printf("ERROR the symbol is not an identifier\n");	/*fprintf(output, "ERROR the symbol is not an identifier\n");*/		return -1;		
.	       				printf("ERROR symbol can not be tokenized\n");				/*fprintf(output, "ERROR symbol can not be tokenized\n");*/	return -1;
%% 

int yywrap()
{
	
} 
/*int main(int argc, char **argv) 
{
	output = fopen("parsed_cpp.txt", "w");

	if(argc >= 2){
		yyin = fopen(argv[1], "r");
		yylex(); 
		fclose(yyin);
	}
	else
		yylex(); 

	fclose(output);
	
	return 0; 

	//To compile:
	//lex gpp_lexer.l
	//gcc lex.yy.c
	//To run:
	//./a.out

} */

