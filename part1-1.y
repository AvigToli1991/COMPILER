%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include"lex.yy.c"


typedef struct node
{
	char *token;
	struct node *left;
	struct node *right;
} node;

node* mknode(char* token, node *left, node *right);
void printTree(node *tree);

void printSpace(int n);

int yylex();
int yyerror(char *e);

%}

%union
{
    struct node *node;
    char *string;
}


%token<string> COMMENT
%token<string> BOOL INT REAL STRING CHAR VAR ARG
%token<string> INTPTR CHARPTR REALPTR
%token<string> IF ELSE DO WHILE FOR FUNC VOID RETURN 
%token<string> AND OR EQ GTE LTE NOTEQ NOT
%token<string> ID 
%token<string> STR_VAL REAL_VAL CHAR_VAL NUL DEC_VAL HEX_VAL BOOLVAL


/*
%type <node> project program ret expr
%type <node> params parameters par value var_dec declars declartions statements
%type <node> proc procedures functype proctype body
*/

%type <node> program
%type <node> processes
%type <node> process
%type <node> arg
%type <node> args
%type <node> argss
%type <node> prosSplit
%type <node> ids
%type <node> type
%type <node> funccall
%type <node> funcargs
%type <node> moreargs
%type <node> vardic
%type <node> varsdic
%type <node> varasign
%type <node> ex
%type <node> op
%type <node> val
%type <node> lop
%type <node> forlop
%type <node> init
%type <node> intval
%type <node> expression
%type <node> not
%type <node> notexp
%type <node> splitnot
%type <node> lopexp
%type <node> oper
%type <node> update
%type <node> updatechage
%type <node> lopbody body bodysplit bodyret retval
%type <node> whilelop
%type <node> dolop ifstate elstate project
%type<string>cmt














%start project

%%

project: cmt program {printTree($1);}     
;

cmt: COMMENT cmt | 
;

program: processes {$$=mknode("CODE",$1,mknode(")",NUL,NUL));} 
;


processes: process processes {$$=mknode("",$1,$2);} 
			    			| {$$=NUL;}
;

process: FUNC ID '(' args ')' ':' prosSplit 

											{$$=mknode("FUNC",
															mknode("PRINT_TOKEN",
																	mknode($2,NUL,NUL),
																	mknode("\n",$4,$7)),				
															mknode(")",NUL,NUL));}
;

prosSplit: type '{' bodyret '}' 
		
								{$$=mknode("\n",
												mknode("RET",
														mknode("PRINT_TOKEN",$1,NUL),
														mknode(")",NUL,NUL)),
												mknode("\n",
															mknode("BODY",
																		$3,
																		mknode(")",NUL,NUL))));} 
								
			| VOID '{' body '}'    

								{$$=mknode("\n",
											mknode("RET",
													mknode("PRINT_TOKEN",mknode("VOID",NUL,NUL),NUL),
													mknode(")",NUL,NUL)),
											mknode("\n",
														mknode("BODY",
																	$3,
																	mknode(")",NUL,NUL))));} 
;
	  


args: arg argss {$$=mknode("ARGS",
								mknode("",$1,$2),
								mknode(")",NUL,NUL));}
				| {$$=mknode("ARGS",
								mknode("PRINT_TOKEN",
													mknode("NONE",NUL,NUL),
													NUL),
								mknode(")",NUL,NUL));}

; 

argss: ';' arg argss {$$=mknode("",$2,NUL);}  
	      | {$$=NUL;}
;

arg: ARG ID ids ':' type  {$$=mknode("",
										$5,
										mknode("PRINT_TOKEN",
															mknode($2,$3,NUL),NUL));}
;

ids: ','ID ids {$$=mknode("PRINT_TOKEN",mknode($2,$3,NUL),NUL);}
              | {$$=NUL;}
;

type: INT     {$$=mknode("INT",NUL,NUL);}    | 
      REAL    {$$=mknode("REAL",NUL,NUL);}   | 
      BOOL    {$$=mknode("BOOL",NUL,NUL);}   | 
      STRING  {$$=mknode("STRING",NUL,NUL);} | 
      CHAR    {$$=mknode("CHAR",NUL,NUL);}   |
      INTPTR  {$$=mknode("INTPTR",NUL,NUL);} |
      CHARPTR {$$=mknode("CHARPTR",NUL,NUL);}| 
      REALPTR {$$=mknode("REALPTR",NUL,NUL);}
;    	

funccall: ID '(' funcargs ')'';' 

								{$$=mknode("CALL FUNC", 
													mknode("PRINT_TOKEN",
																		mknode($1,mknode("\n",NUL,NUL),NUL),
																		mknode("PARAMS",
																						$3,
																						mknode(")",NUL,NUL))
																						));}
;

funcargs:ID moreargs 
						{$$=mknode("PRINT_TOKEN",mknode($1,NUL,NUL),$2);}
		   			|  {$$=NUL;}
;

moreargs: ',' ID moreargs {$$=mknode("PRINT_TOKEN",mknode($1,NUL,NUL),$2);}
			| {$$=NUL;}
;

vardic: VAR ID varasign varsdic ':' type ';' 
						{$$=mknode("DECLARATION",
												$6,
												mknode("\n",
															mknode("=",
																		$3,
																		mknode($2,
																					$4,
																					mknode(")",NUL,NUL))),
															NUL));}
		
;

varsdic: ',' ID varasign varsdic 
									{$$=mknode("=",
													$3,
													mknode($2,
																$4,
																mknode(")",NUL,NUL)));}
								| {$$=NUL;}
;

varasign: '=' ex {$$=mknode("",$2,NUL);}| {$$=NUL;}
;
      
ex: '('ex')' {$$=mknode("",$2,NUL,NUL)}| 
	val      {$$=mknode("PRINT_TOKEN",$1,NUL);}|
    ID       {$$=mknode("PRINT_TOKEN",mknode($1,NUL,NUL),NUL);}|
	funccall {$$=mknode("",$1,NUL);} |
    ex op ex 
				{$$=mknode("OPERATION",
										mknode("PRINT_TOKEN",
															$2,
															mknode("",$1,$3)),
										mknode(")",NUL,NUL)) ;} 
;

op: '+' {$$=mknode("+",NUL,NUL);}|
    '-' {$$=mknode("-",NUL,NUL);}| 
    '*' {$$=mknode("*",NUL,NUL);}|
    '/' {$$=mknode("/",NUL,NUL);}
;

val: STR_VAL  {$$=mknode($1,NUL,NUL)}|
     REAL_VAL {$$=mknode($1,NUL,NUL)}| 
     CHAR_VAL {$$=mknode($1,NUL,NUL)}| 
     DEC_VAL  {$$=mknode($1,NUL,NUL)}|
     HEX_VAL  {$$=mknode($1,NUL,NUL)}|
     BOOLVAL  {$$=mknode($1,NUL,NUL)}      
     
;

lop: forlop   {$$=mknode("",$1,NUL)}      |
     whilelop {$$=mknode("",$1,NUL)}      |
     dolop    {$$=mknode("",$1,NUL)}
;
forlop: FOR '(' init ';' expression ';' update ')' '{'lopbody'}'
	{$$=mknode("FOR",
					mknode("INIT",
								mknode("",
										$3,
										mknode("\n",
													mknode("EXPRESSION",
																		$5,
																		mknode(")",NUL,NUL)),
													mknode("\n",
																mknode("UPDATE",
																				$7,
																				mknode(")",NUL,NUL)),
																mknode("\n",
																			mknode($9,$10,NUL),
																			NUL))),
								mknode(")",NUL,NUL)),
					mknode(")",NUL,NUL)));}
;

init: ID '=' intval {$$ = mknode("=",
									mknode("",
												$3,
												mknode(")",NUL,NUL)),
									mknode($1,NUL,NUL));}
;
intval: DEC_VAL {$$=mknode($1,NUL,NUL);} | 
		ID		 {$$=mknode($1,NUL,NUL);}
;
expression: '(' expression ')' lopexp     {$$=mknode("",$2,$3);} |
            notexp lopexp    			{$$=mknode("",$1,$2);} |
			ex oper ex  lopexp		    {$$=mknode("OPERATION",
																mknode("",
																			$2,
																			mknode("",
																						$1,
																						mknode("",
																									$3,
																									NUL))),
																mknode(")",NUL,NUL));}  
;

not: NOT {$$ = mknode($1)} | {$$=NUL;}
;

notexp: NOT splitnot {$$=mknode("PRINT_TOKEN",mknode($1,$2,NUL),NUL);}
;

splitnot: ex                    {$$=mknode("",$1,NUL);}  | 
	 	  '(' expression ')' 	{$$=mknode("",$2,NUL);} 
;


lopexp: oper expression lopexp
								{$$=mknode("OPERATION",
														mknode("",
																	$2,
																	mknode("",$1,$3)),
														mknode(")",NUL,NUL)) ;}
							|   {$$=NUL;}
;

oper: EQ    {$$=mknode($1,NUL,NUL);}|
      GTE   {$$=mknode($1,NUL,NUL);}|
      LTE   {$$=mknode($1,NUL,NUL);}|
      NOTEQ {$$=mknode($1,NUL,NUL);}|
      '>'   {$$=mknode($1,NUL,NUL);}|
      '<'   {$$=mknode($1,NUL,NUL);}|
	  AND   {$$=mknode($1,NUL,NUL);}| 
      OR    {$$=mknode($1,NUL,NUL);}
;



update:ID updatechage {$$=mknode("UPDATE",
										mknode("PRINT_TOKEN",$2,NUL),
										mknode(")",NUL,NUL));}
;

updatechage: '+''+'          {$$=mknode("+1",NUL,NUL);}		         |
	          '-''-'         {$$=mknode("-1",NUL,NUL);}		         |
	          '*''=' DEC_VAL {$$=mknode("*",$3,NUL);} 					 |
	          '+''=' DEC_VAL {$$=mknode("+",$3,NUL);} 					 |
	          '-''=' DEC_VAL {$$=mknode("-",$3,NUL);} 
;

lopbody: body    {$$=mknode("",$1,NUL);}|
	 bodyret {$$=mknode("",$1,NUL);}
;

whilelop: WHILE '(' expression ')' '{'lopbody'}'
			{$$=mknode("WHILE",
								mknode("EXPRESSION",
													mknode("",
																$3,
																mknode("{}",
																			$6,
																			mknode(")",NUL,NUL))),
													mknode(")",NUL,NUL)),
								mknode(")",NUL,NUL));}
;

dolop: DO '{' lopbody '}' WHILE '(' expression ')'
	{$$=mknode("DO-WHILE",
					mknode("{}",
								mknode("",
										$3,
										mknode(")",
													mknode("EXPRESSION",
																			$7,
																			mknode(")",NUL,NUL)),
													NUL)),
								NUL),
					mknode(")",NUL,NUL));}
;

ifstate: IF '(' expression ')' '{' lopbody '}' elstate 
	{$$ = mknode("IF-ELSE",
						mknode("EXPRESSION",
											$3,
											mknode("{}",
														mknode("",
																$6,
																mknode(")",NUL,NUL)),
														$8)),
						mknode(")",NUL,NUL));}
;

elstate: '{'lopbody'}'  {$$=mknode("{}",
										mknode("",$2,NUL),
										mknode(")",NUL,NUL));}
			|{$$=NUL;} 
;     
      

body: cmt bodysplit {$$=mknode("",$2,NUL);}

bodysplit: process      body  {$$=mknode("",$1,$2);}|
           ifstate      body  {$$=mknode("",$1,$2);}|
           vardic       body  {$$=mknode("",$1,$2);}|
           lop          body  {$$=mknode("",$1,$2);}|
      	   funccall     body  {$$=mknode("",$1,$2);}|
      	   '{' body '}' body  {$$=mknode("{}",
      	   									mknode("",$2,$4),
											mknode(")",NUL,NUL));}
							| {$$=NUL;}
;

bodyret:body RETURN retval {$$=mknode("",
										$1,
										mknode("RETURN_VAL",
															$3,
															mknode(")",NUL,NUL)));}
;

retval: ID         {$$=mknode("PRINT_TOKEN",mknode($1,NUL,NUL),NUL);} |
        funccall   {$$=mknode("",$1,NUL);} |
        val        {$$=mknode("",$1,NUL);} |
        expression {$$=mknode("EXPRESSION",
											$1,
											mknode(")",NUL,NUL));}
;
 

%%
int main()
{
	return yyparse(); 
}

/* allocation for node*/
node* mknode (char *token, node *left, node *right)
{
	node *newnode = (node*)malloc(sizeof(node));
	newnode->left=left;
	newnode->right=right;
	newnode->token=token;
	return newnode;
}

void printSpace(int num){
	printf("%s"," "*num);
}
void printNodesTree(node* tree){
	if(tree != NUL){

	}
}
void printTree(node* tree)
{	
	
	if (tree != NUL){
		if((strcmp(tree->token,"CODE") == 0 )        ||
		(strcmp(tree->token, "FUNC") == 0)        ||
		(strcmp(tree->token, "BODY") == 0)        ||
		(strcmp(tree->token, "ARGS") == 0)        ||
		(strcmp(tree->token, "{}") == 0)          ||
		(strcmp(tree->token, "RETURN_VAL") == 0)  || 
		(strcmp(tree->token, "CALL FUNC") == 0)   ||
		(strcmp(tree->token, "PARAMS") == 0)		 ||
		(strcmp(tree->token, "RET") == 0)		 ||
		(strcmp(tree->token,"DECLARATION") == 0 ) ||
		(strcmp(tree->token,"=") == 0)            ||
		(strcmp(tree->token,"FOR") == 0)		     ||
		(strcmp(tree->token,"WHILE") == 0)		 ||
		(strcmp(tree->token,"DO-WHILE") == 0)	 ||
		(strcmp(tree->token,"INIT") == 0)         ||
		(strcmp(tree->token,"EXPRESSION") == 0))
		{
			
			printSpace(spaces);
			
			if(strcmp(tree->token, "{}") == 0){

				printf("(BLOCK \n");
			}
			else if (strcmp(tree->token, "=") == 0){

				if(tree->left != NUL){
					printf("(%s %s\n",tree->token,tree->right->token);
				}
				else{

					printf("%s \n",tree->right->token);
					spaces -= 4;;
				}
			}
			else if(strcmp(tree->token,"OPERATION")){

				printf("(%s \n",tree->left->left->token);

			}
			else{

				printf("(%s \n",tree->token);
			}
			
			spaces += 4;
			printSpace(spaces);
			
		}

		else if (strcmp(tree->token, "PRINT_TOKEN") == 0)
		{

			printf("%s ",tree->left->token);
		}

		else if (strcmp(tree->token, "\n") == 0){
			printf("\n");
			printSpace(spaces);
		}

		else ((strcmp(tree->token,"STR_VAL") == 0)    ||
			(strcmp(tree->token,"REAL_VAL") == 0)    ||
			(strcmp(tree->token,"CHAR_VAL") == 0)    ||
			(strcmp(tree->token,"DEC_VAL") == 0)     ||
			(strcmp(tree->token,"HEX_VAL") == 0)     ||
			(strcmp(tree->token,"BOOLVAL") == 0)    ||
			(strcmp(tree->token,"BOOL") == 0)        ||
			(strcmp(tree->token,"INT") == 0)         ||
			(strcmp(tree->token,"REAL") == 0)        ||
			(strcmp(tree->token,"STRING") == 0)      ||
			(strcmp(tree->token,"CHAR") == 0)        ||
			(strcmp(tree->token,"INTPTR") == 0)     ||
			(strcmp(tree->token,"CHARPTR") == 0)    ||
			(strcmp(tree->token,"REALPTR") == 0))
		{
			printf("%s",tree->token);
		}
		printTree(tree->left);
		printTree(tree->right);
	}
	else{
		printf("NO CODE TO PRESENT");
	}
	
	
}

int yyerror(char *e)
{
	int yydebug=1;
	fflush(stdout);
	fprintf(stderr,"Error %s at line %d\n" ,e,yylineno);
	fprintf(stderr, "does not accept '%s'\n",yytext);

	return 0;
} 


