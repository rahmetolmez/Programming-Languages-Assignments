%{ 
	FILE* output;
	int quotationCounter = 0;
%} 

%option caseless

NON_ZERO_DIGIT			[1-9]
DIGIT 				[0-9]*
IDENTIFIER	    		[a-z][a-z0-9]*

%%
0						printf("VALUE\n"); 							fprintf(output, "VALUE\n");
0*						printf("ERROR the symbol is not a value\n");				fprintf(output, "ERROR the symbol is not a value\n");
{NON_ZERO_DIGIT}{DIGIT}	printf("VALUE\n");										fprintf(output, "VALUE\n");	
";;"[^\n]*""					printf("COMMENT\n");							fprintf(output, "COMMENT\n");
and						printf("KW_AND\n");							fprintf(output, "KW_AND\n");
or 						printf("KW_OR\n");							fprintf(output, "KW_OR\n");
not						printf("KW_NOT\n");							fprintf(output, "KW_NOT\n");
equal						printf("KW_EQUAL\n");							fprintf(output, "KW_EQUAL\n");
less						printf("KW_LESS\n");							fprintf(output, "KW_LESS\n");
nil						printf("KW_NIL\n");							fprintf(output, "KW_NIL\n");
list						printf("KW_LIST\n");							fprintf(output, "KW_LIST\n");
append						printf("KW_APPEND\n");							fprintf(output, "KW_APPEND\n");
concat						printf("KW_CONCAT\n");							fprintf(output, "KW_CONCAT\n");
set						printf("KW_SET\n");							fprintf(output, "KW_SET\n");
deffun						printf("KW_DEFFUN\n");							fprintf(output, "KW_DEFFUN\n");
for						printf("KW_FOR\n");							fprintf(output, "KW_FOR\n");
if						printf("KW_IF\n");							fprintf(output, "KW_IF\n");
exit						printf("KW_EXIT\n");							fprintf(output, "KW_EXIT\n");
load						printf("KW_LOAD\n");							fprintf(output, "KW_LOAD\n");
disp						printf("KW_DISP\n");							fprintf(output, "KW_DISP\n");
true						printf("KW_TRUE\n");							fprintf(output, "KW_TRUE\n");
false						printf("KW_FALSE\n");							fprintf(output, "KW_FALSE\n");
"+"						printf("OP_PLUS\n");							fprintf(output, "OP_PLUS\n");
"-"						printf("OP_MINUS\n");							fprintf(output, "OP_MINUS\n");
"/"						printf("OP_DIV\n");							fprintf(output, "OP_DIV\n");
"*"						printf("OP_MULT\n");							fprintf(output, "OP_MULT\n");
"("						printf("OP_OP\n");							fprintf(output, "OP_OP\n");
")"						printf("OP_CP\n");							fprintf(output, "OP_CP\n");
"**"						printf("OP_DBLMULT\n");							fprintf(output, "OP_DBLMULT\n");	
"“"						printf("OP_OC\n");							fprintf(output, "OP_OC\n");	
"”"						printf("OP_CC\n");							fprintf(output, "OP_CC\n");	
"\""						quotationCounter++; if(quotationCounter % 2 == 1) {printf("OP_OC\n"); 	fprintf(output, "OP_OC\n");} else {printf("OP_CC\n"); fprintf(output, "OP_CC\n");}	
","						printf("OP_COMMA\n");							fprintf(output, "OP_COMMA\n");	
" "
\n
\r
\t
{IDENTIFIER}					printf("IDENTIFIER\n");  						fprintf(output, "IDENTIFIER\n");
{IDENTIFIER}+[^{IDENTIFIER}" "\n\r\t"("")""\""]+ printf("ERROR the  d symbol is not an identifier\n");			fprintf(output, "ERROR the symbol is not an identifier\n");		
\n\n	 				return 0;
[0-9]+[a-z]*					printf("ERROR the symbol is not an identifier\n");			fprintf(output, "ERROR the symbol is not an identifier\n");				
.	       					printf("ERROR symbol can not be tokenized\n");				fprintf(output, "ERROR symbol can not be tokenized\n");
%% 

int yywrap(){} 
int main(int argc, char **argv) 
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

} 

