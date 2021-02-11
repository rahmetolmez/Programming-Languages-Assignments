%{
	#include <stdio.h>
	#include <stdlib.h>	
	#include <string.h>
	
	#define YYDEBUG 1

	extern int yylex();
	void yyerror(const char *msg);
	int power(int a, int b);
	int findIdentifier(char* name);
	void initList();
	void addToList(int list[], int num);
	void printList(int list[]);
	void copyListTo(int from[], int to[]);

	struct variable
	{
		char* name;
		int integer;
		int list[1000];
		int flag;
	}variable;

	struct variable table[100];

	extern FILE *yyin;
	FILE* outFile;
	int ind = 0;
%}

%union
{
	struct
	{
		int integer;
		char* name;
		int list[1000];
		int index;
		int flag;

	}type;
};

%start START
%token<type> IDENTIFIER
%token<type> KW_AND KW_OR KW_NOT KW_EQUAL KW_TRUE KW_FALSE
%token<type> KW_SET KW_DEFFUN
%token<type> KW_FOR KW_IF
%token<type> KW_APPEND KW_CONCAT KW_LIST
%token<type> KW_LESS KW_NIL KW_LOAD KW_EXIT KW_DISP
%token<type> OP_PLUS OP_MINUS OP_DIV OP_MULT OP_DBLMULT
%token<type> OP_OP OP_CP OP_OC OP_CC OP_COMMA
%token<type> VALUE
/*%token<type> COMMENT*/
%token<type> KW_WHILE /*KW_DEFVAR*/ BinaryValue
%token ERROR

%type<type> EXPI EXPB EXPLISTI IDLIST LISTVALUE VALUES

%%

START
	: INPUTS
	;

INPUTS
	: INPUTS INPUT
	| INPUT
	;

INPUT
	: EXPI		/*{printf("final name: %s, num: %d\n", $1.name, $1.integer);}*/
	| EXPLISTI	/*{printf("final name: %s, num: %d\n", $1.name, $1.integer);}*/
	;

EXPI
	: OP_OP OP_PLUS EXPI EXPI OP_CP 	{$$.integer = $3.integer + $4.integer; printf(">> SUM: %d\n", $$.integer);}
	| OP_OP OP_MINUS EXPI EXPI OP_CP 	{$$.integer = $3.integer - $4.integer; printf(">> SUB: %d\n", $$.integer);}
	| OP_OP OP_MULT EXPI EXPI OP_CP 	{$$.integer = $3.integer * $4.integer; printf(">> MULT: %d\n", $$.integer);}
	| OP_OP OP_DBLMULT EXPI EXPI OP_CP 	{$$.integer = power($3.integer , $4.integer); printf(">> DBLMULT: %d\n", $$.integer);}
	| OP_OP OP_DIV EXPI EXPI OP_CP 		{$$.integer = $3.integer / $4.integer; printf(">> DIV: %d\n", $$.integer);}
	| IDENTIFIER						{
											if(findIdentifier($1.name) == -1)		/*if identifier does not exist*/
											{
												$$.name = $1.name; 
												$$.integer = $1.integer; 
												printf(">> IDENTIFIER NOT INITIALIZED\n");
											}
											else									/*if identifier already exists*/
											{

												$$.name = strdup($1.name);
												if(table[findIdentifier($1.name)].flag == 3)
												{
													$$.flag = 3;
													copyListTo(table[findIdentifier($1.name)].list, $$.list);
													printf(">> %s = ", $1.name);
													printList($$.list);
													

												}
												else
												{
													$$.integer = table[findIdentifier($1.name)].integer; /*get value*/
													int temp = findIdentifier($1.name);
													printf(">> IDENTIFIER %s as %d\n", $1.name, $$.integer);
												}
												
											}

											/*$$.name = $1.name; 
											$$.integer = $1.integer;*/ 
											/*printf("IDENTIFIER: %s = %d\n", $1.name, $1.integer);*/
										}
	| VALUE 							{$$.integer = $1.integer;}
	| OP_OP IDENTIFIER EXPLISTI OP_CP	{;} /*function call*/
	;

EXPI
	: OP_OP KW_SET IDENTIFIER EXPI OP_CP	{
												if($4.flag == 3)								/*if a list*/
												{
													$$ = $4;
													$$.flag = 3;
													printf(">> SET %s as ", $3.name);
													printList($4.list);

													if(findIdentifier($3.name) == -1)			/*if identifier does not exist*/
													{
														table[ind].name = strdup($3.name);		/*add new variable to table*/
														table[ind].flag = 3;
														copyListTo($$.list, table[ind].list);	/*bind value*/
														ind++; 
													}
													else
													{
														copyListTo($$.list, table[findIdentifier($3.name)].list);	/*update value*/
														int temp = findIdentifier($3.name);

													}
												}
												else
												{
													$$.integer = $4.integer; 
													if(findIdentifier($3.name) == -1)		/*if identifier does not exist*/
													{
														table[ind].name = strdup($3.name);	/*add new variable to table*/
														table[ind].integer = $$.integer;	/*bind value*/
														printf(">> SET %s as %d\n", $3.name, $$.integer);
														ind++; 
													}
													else
													{
														table[findIdentifier($3.name)].integer = $$.integer; /*update value*/
														int temp = findIdentifier($3.name);
														printf(">> SET %s as %d\n", $3.name, $$.integer);
													}
												}
												
											}
	;

EXPI
	: OP_OP KW_DEFFUN IDENTIFIER IDLIST EXPLISTI OP_CP	{$$.integer = 0; printf("FUNCTION DEFINED\n");}
	;

EXPI /*should be OP_OP KW_IF EXPB EXPI OP_CP*/
	: OP_OP KW_IF EXPB EXPI OP_CP	{if($3.integer == 0) printf(">> IF STATEMENT SHOULD NOT BE EVALUATED\n"); else printf(">> IF STATEMENT SHOULD BE EVALUATED\n");}
	;

/*EXPI
	: OP_OP KW_IF EXPB EXPLISTI EXPLISTI OP_CP 
	;*/

EXPI /*should be OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPI OP_CP*/
	: OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPI OP_CP
	;

EXPLISTI
	: OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP
	| OP_OP KW_APPEND EXPI EXPLISTI OP_CP
	/**| LISTVALUE**/
	| KW_NIL
	;

EXPB
	: OP_OP KW_AND EXPB EXPB OP_CP		{if($3.integer == 0 || $4.integer == 0) $$.integer = 0; else $$.integer = 1; printf(">> AND RESULT %d\n", $$.integer);}
	| OP_OP KW_OR EXPB EXPB OP_CP		{if($3.integer == 0 && $4.integer == 0) $$.integer = 0; else $$.integer = 1; printf(">> OR RESULT %d\n", $$.integer);}
	| OP_OP KW_NOT EXPB OP_CP			{if($3.integer == 0) $$.integer = 1; else $$.integer = 0; printf(">> NOT RESULT %d\n", $$.integer);}
	| OP_OP KW_EQUAL EXPB EXPB OP_CP	{if($3.integer == $4.integer) $$.integer = 1; else $$.integer = 0;}
	| OP_OP KW_EQUAL EXPI EXPI OP_CP	{if($3.integer == $4.integer) $$.integer = 1; else $$.integer = 0;}
	| VALUE/*changed from BinaryValue*/	{$$.integer = $1.integer;}
	;

/*NOT SURE*/
IDLIST
	: OP_OP IDENTIFIERS OP_CP
	| OP_OP OP_CP
	| KW_NIL					{$$.integer = 0;}
	;

IDENTIFIERS
	: IDENTIFIERS IDENTIFIER
	| IDENTIFIER
	;

EXPI
	: OP_OP KW_LIST VALUES OP_CP	{
										$$ = $3;
										$$.flag = 3;		/*list flag*/
										printf(">> ");
										printList($3.list);
									}
	;

LISTVALUE
	: OP_OP VALUES OP_CP
	| OP_OP OP_CP
	| KW_NIL				{$$.integer = 0;}
	;
	
VALUES
	: VALUES VALUE	{	
						addToList($$.list, $2.integer);
						/*addToList($$.list, $2.integer);*/
						/*$$.list = strdup(merge($1.list, $2.list));*/
						/*for(int i = 0; i < 3; i++) $1.list[i] = $2.list[i];
						memcpy($$.list, $1.list, sizeof($1.list));
						printf("so far: %d %d %d\n", $$.index, $$.list[1], $$.list[2]);*/
					}

	| VALUE			{	
						addToList($$.list, $1.integer);
						/*$1.list[0] = 
						$$.list = updateList();
						$$.integer = $1.integer; $$.list[$$.index] = $1.integer; $$.index++; printf("list: %d\n", $$.list[0]);*/
					}
	;

%%


int findIdentifier(char* name)
{
	char * second = strdup(name);

	for(int i = 0; i < 100; i++)
	{
		if(strcmp(table[i].name, second) == 0)
			return i;
	}
	return -1;
}

int power(int a, int b)
{
	int result = a;
	for(int i = 0; i < b - 1; i++)
		result = result * a;
	return result;
}

void addToList(int list[], int num)
{
	int i = 0;
	while(list[i] != '\0')
	{
		i++;
	}
	list[i] = num;
	list[i + 1] = '\0';
}

void printList(int list[])
{
	int i = 0;
	printf("( ");
	while(list[i] != '\0')
	{
		printf("%d ", list[i]);
		i++;
	}
	printf(")\n");
}

void copyListTo(int from[], int to[])
{
	int i = 0;
	while(from[i] != '\0')
	{
		to[i] = from[i];
		i++;
	}
	to[i] = '\0';
}

void defineIdentifier()
{
	
}

void defineFunction()
{
	
}

void callFunction()
{
	
}

void yyerror(const char *msg)
{
	fprintf(stderr, "%s\n", msg);
	exit(1);
}

int main(int argc, char** argv)
{
	outFile = fopen("parsed_cpp.txt", "w");

	for(int i = 0 ; i < 100; i++) 
	{  
		table[i].name = (char*)malloc(50 * sizeof(char));
	}

	if(argc >= 2){
		yyin = fopen(argv[1], "r");
		yyparse(); 
		fclose(yyin);
	}
	else	
		yyparse();
	
	fclose(outFile);
	return 0;
}





/*not in lexer EXPI
	: OP_OP KW_DEFVAR IDENTIFIER EXPI OP_CP
	;
*/

/*VALUES
	: VALUES VALUE
	| VALUE
	;*/

/*LISTVALUE
	: '\'' OP_OP VALUES OP_CP
	| '\'' OP_OP OP_CP
//	| KW_NIL
	;*/

/*EXPI
	: OP_OP KW_SET IDENTIFIER  EXPI OP_CP
	;*/

/*EXPI
	: OP_OP IDENTIFIER EXPLISTI OP_CP
	;*/

/*EXPI
	: OP_OP KW_WHILE OP_OP EXPB OP_CP EXPLISTI OP_CP //not in lexer!
	;*/