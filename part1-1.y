%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct node
{
	char *token;
	struct node *left;
	struct node *right;
} node;

node* mknode(char *token, node *left, node *right);
void printTree(node *tree);

void printSpace(int n);

int yylex();
int yyerror(char *e);
int spaces=0;
%}

%union
{
    struct node *node;
    char *string;
}


%token<string> COMMENT LENGTH
%token<string> BOOL INT REAL STRING CHAR VAR ARG
%token<string> INTPTR CHARPTR REALPTR
%token<string> IF ELSE DO WHILE FOR FUNC VOID RETURN 
%token<string> AND OR EQ GTE LTE NOTEQ NOT GT LT ASS
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

program: processes {$$=mknode("CODE",$1,mknode(")",NULL,NULL));} 
;


processes: process processes {$$=mknode("",$1,$2);} 
			    			| {$$=NULL;}
;

process: FUNC ID '(' args ')' ':' prosSplit 

											{$$=mknode("FUNC",
															mknode("PRINT_TOKEN",
																	mknode($2,NULL,NULL),
																	mknode("\n",$4,$7)),				
															mknode(")",NULL,NULL));}
;

prosSplit: type '{' bodyret '}' 
		
								{$$=mknode("\n",
												mknode("RET",
														mknode("PRINT_TOKEN",$1,NULL),
														mknode(")",NULL,NULL)),
												mknode("\n",
															mknode("BODY",
																		$3,
																		mknode(")",NULL,NULL)),NULL));} 
								
			| VOID '{' body '}'    

								{$$=mknode("\n",
											mknode("RET",
													mknode("PRINT_TOKEN",mknode("VOID",NULL,NULL),NULL),
													mknode(")",NULL,NULL)),
											mknode("\n",
														mknode("BODY",
																	$3,
																	mknode(")",NULL,NULL)),NULL));} 
;
	  


args: arg argss {$$=mknode("ARGS",
								mknode("",$1,$2),
								mknode(")",NULL,NULL));}
				| {$$=mknode("ARGS",
								mknode("PRINT_TOKEN",
													mknode("NONE",NULL,NULL),
													NULL),
								mknode(")",NULL,NULL));}

; 

argss: ';' arg argss {$$=mknode("",$2,NULL);}  
	      | {$$=NULL;}
;

arg: ARG ID ids ':' type  {$$=mknode("",
										$5,
										mknode("PRINT_TOKEN",
															mknode($2,$3,NULL),NULL));}
;

ids: ','ID ids {$$=mknode("PRINT_TOKEN",mknode($2,$3,NULL),NULL);}
              | {$$=NULL;}
;

type: INT     {$$=mknode("INT",NULL,NULL);}    | 
      REAL    {$$=mknode("REAL",NULL,NULL);}   | 
      BOOL    {$$=mknode("BOOL",NULL,NULL);}   | 
      STRING  {$$=mknode("STRING",NULL,NULL);} | 
      CHAR    {$$=mknode("CHAR",NULL,NULL);}   |
      INTPTR  {$$=mknode("INTPTR",NULL,NULL);} |
      CHARPTR {$$=mknode("CHARPTR",NULL,NULL);}| 
      REALPTR {$$=mknode("REALPTR",NULL,NULL);}
;    	

funccall: ID '(' funcargs ')'';' 

								{$$=mknode("CALL FUNC", 
													mknode("PRINT_TOKEN",
																		mknode($1,mknode("\n",NULL,NULL),NULL),
																		mknode("PARAMS",
																						$3,
																						mknode(")",NULL,NULL))
																						),NULL);}
;

funcargs:ID moreargs 
						{$$=mknode("PRINT_TOKEN",mknode($1,NULL,NULL),$2);}
		   			|  {$$=NULL;}
;

moreargs: ',' ID moreargs {$$=mknode("PRINT_TOKEN",mknode($2,NULL,NULL),$3);}
			| {$$=NULL;}
;

vardic: VAR ID varasign varsdic ':' type ';' 
						{$$=mknode("DECLARATION",
												$6,
												mknode("\n",
															mknode("=",
																		$3,
																		mknode($2,
																					$4,
																					mknode(")",NULL,NULL))),
															NULL));}
		
;

varsdic: ',' ID varasign varsdic 
									{$$=mknode("=",
													$3,
													mknode($2,
																$4,
																mknode(")",NULL,NULL)));}
								| {$$=NULL;}
;

varasign: '=' ex {$$=mknode("",$2,NULL);}| {$$=NULL;}
;
      
ex: '('ex')' {$$=mknode("",$2,NULL)}| //removes one of the NULLs (the ritgh one)
	val      {$$=mknode("PRINT_TOKEN",$1,NULL);}|
    ID       {$$=mknode("PRINT_TOKEN",mknode($1,NULL,NULL),NULL);}|
	funccall {$$=mknode("",$1,NULL);} |
    ex op ex 
				{$$=mknode("OPERATION",
										mknode("PRINT_TOKEN",
															$2,
															mknode("",$1,$3)),
										mknode(")",NULL,NULL)) ;} 
;

op: '+' {$$=mknode("+",NULL,NULL);}|
    '-' {$$=mknode("-",NULL,NULL);}| 
    '*' {$$=mknode("*",NULL,NULL);}|
    '/' {$$=mknode("/",NULL,NULL);}
;

val: STR_VAL  {$$=mknode($1,NULL,NULL)}|
     REAL_VAL {$$=mknode($1,NULL,NULL)}| 
     CHAR_VAL {$$=mknode($1,NULL,NULL)}| 
     DEC_VAL  {$$=mknode($1,NULL,NULL)}|
     HEX_VAL  {$$=mknode($1,NULL,NULL)}|
     BOOLVAL  {$$=mknode($1,NULL,NULL)}      
     
;

lop: forlop   {$$=mknode("",$1,NULL)}      |
     whilelop {$$=mknode("",$1,NULL)}      |
     dolop    {$$=mknode("",$1,NULL)}
;
forlop: FOR '(' init ';' expression ';' update ')' '{'lopbody'}'
	/* {$$=mknode("FOR",
					mknode("INIT",
								mknode("",
										$3,
										mknode("\n",
													mknode("EXPRESSION",
																		$5,
																		mknode(")",NULL,NULL)),
													mknode("\n",
																mknode("UPDATE",
																				$7,
																				mknode(")",NULL,NULL)),
																mknode("\n",
																			mknode("{}",$10,NULL),
																			NULL))),
								mknode(")",NULL,NULL)),
					mknode(")",NULL,NULL)));} */
	{
		$$=mknode("FOR" ,
						mknode("INIT" ,
										mknode("",
												$3,
												mknode("\n",
															mknode("EXPRESSION" ,
																				$5,
																				mknode(")",NULL,NULL)),
															mknode("\n",
																		mknode("UPDATE",
																						$7,
																						mknode(")",NULL,NULL)),
																		mknode("\n",
																					mknode("{}",
																								$10,
																								NULL),
																					NULL)))),
										mknode(")" , NULL,NULL)),
						mknode(")",NULL,NULL));
						
									
	}
;

init: ID '=' intval {$$ = mknode("=",
									mknode("",
												$3,
												mknode(")",NULL,NULL)),
									mknode($1,NULL,NULL));}
;
intval: DEC_VAL {$$=mknode($1,NULL,NULL);} | 
		ID		 {$$=mknode($1,NULL,NULL);}
;
expression: '(' expression ')' lopexp     {$$=mknode("(",$2,$4);} |
            notexp lopexp    			{$$=mknode(" ",$1,$2);} |
			ex oper ex  lopexp		    {$$=mknode("OPERATION",
																mknode("",
																			$2,
																			mknode("",
																						$1,
																						mknode("",
																									$3,
																									NULL))),
																mknode(")",NULL,NULL));}  
;

not: NOT {$$ = mknode($1,NULL,NULL)} | {$$=NULL;}
;

notexp: NOT splitnot {$$=mknode("PRINT_TOKEN",mknode($1,$2,NULL),NULL);}
;

splitnot: ex                    {$$=mknode("",$1,NULL);}  | 
	 	  '(' expression ')' 	{$$=mknode("",$2,NULL);} 
;


lopexp: oper expression lopexp
								{$$=mknode("OPERATION",
														mknode("",
																	$2,
																	mknode("",$1,$3)),
														mknode(")",NULL,NULL)) ;}
							|   {$$=NULL;}
;

oper: EQ    {$$=mknode($1,NULL,NULL);}|
      GTE   {$$=mknode($1,NULL,NULL);}|
      LTE   {$$=mknode($1,NULL,NULL);}|
      NOTEQ {$$=mknode($1,NULL,NULL);}|
      GT  {$$=mknode($1,NULL,NULL);}|
      LT   {$$=mknode($1,NULL,NULL);}|
	  AND   {$$=mknode($1,NULL,NULL);}| 
      OR    {$$=mknode($1,NULL,NULL);}|
	  ASS    {$$=mknode($1,NULL,NULL);}
;



update:ID updatechage {$$=mknode("UPDATE",
										mknode("PRINT_TOKEN",$2,NULL),
										mknode(")",NULL,NULL));}
;

updatechage: '+''+'          {$$=mknode("+1",NULL,NULL);}		         |
	          '-''-'         {$$=mknode("-1",NULL,NULL);}		         |
	          '*''=' DEC_VAL {$$=mknode("*",$3,NULL);} 					 |
	          '+''=' DEC_VAL {$$=mknode("+",$3,NULL);} 					 |
	          '-''=' DEC_VAL {$$=mknode("-",$3,NULL);} 
;

lopbody: body    {$$=mknode("",$1,NULL);}|
	 bodyret {$$=mknode("",$1,NULL);}
;

whilelop: WHILE '(' expression ')' '{'lopbody'}'
			{$$=mknode("WHILE",
								mknode("EXPRESSION",
													mknode("",
																$3,
																mknode("{}",
																			$6,
																			mknode(")",NULL,NULL))),
													mknode(")",NULL,NULL)),
								mknode(")",NULL,NULL));}
;

dolop: DO '{' lopbody '}' WHILE '(' expression ')'
	{$$=mknode("DO-WHILE",
					mknode("{}",
								mknode("",
										$3,
										mknode(")",
													mknode("EXPRESSION",
																			$7,
																			mknode(")",NULL,NULL)),
													NULL)),
								NULL),
					mknode(")",NULL,NULL));}
;

ifstate: IF '(' expression ')' '{' lopbody '}' elstate 
	{$$ = mknode("IF-ELSE",
						mknode("EXPRESSION",
											$3,
											mknode("{}",
														mknode("",
																$6,
																mknode(")",NULL,NULL)),
														$8)),
						mknode(")",NULL,NULL));}
;

elstate: '{'lopbody'}'  {$$=mknode("{}",
										mknode("",$2,NULL),
										mknode(")",NULL,NULL));}
			|{$$=NULL;} 
;     
      

body: cmt bodysplit {$$=mknode("",$2,NULL);}

bodysplit: process      body  {$$=mknode("",$1,$2);}|
           ifstate      body  {$$=mknode("",$1,$2);}|
           vardic       body  {$$=mknode("",$1,$2);}|
           lop          body  {$$=mknode("",$1,$2);}|
      	   funccall     body  {$$=mknode("",$1,$2);}|
      	   '{' body '}' body  {$$=mknode("{}",
      	   									mknode("",$2,$4),
											mknode(")",NULL,NULL));}
							| {$$=NULL;}
;

bodyret:body RETURN retval {$$=mknode("",
										$1,
										mknode("RETURN_VAL",
															$3,
															mknode(")",NULL,NULL)));}
;

retval: ID         {$$=mknode("PRINT_TOKEN",mknode($1,NULL,NULL),NULL);} |
        funccall   {$$=mknode("",$1,NULL);} |
        val        {$$=mknode("",$1,NULL);} |
        expression {$$=mknode("EXPRESSION",
											$1,
											mknode(")",NULL,NULL));}
;
 

%%


#include"lex.yy.c"


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
	int i = 0;
	while (i < num)
		{
			printf(" ");
			i+=1;
		}
	/* char space = ' ';
	printf("%s",space*num); */
}
/* void printNodesTree(node* tree){
	if(tree != NULL){

	}
} */
void printTree(node* tree)
{	
	
	if (tree != NULL){
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

				if(tree->left != NULL){
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

		else if ((strcmp(tree->token,"STR_VAL") == 0)    ||
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


