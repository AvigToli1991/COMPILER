%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lex.yy.c"

typedef struct node
{
	char *token;
	struct node *left;
	struct node *right;
} node;

node* mknode(char* token, node *left, node *right);
void Printtree(node *tree);

void printTabs(int n);

int yylex();
int yyerror(char *e);

int printlevel=0;
%}

%union
{
    struct node *node;
    char *string;
}


%token COMMENT
%token BOOL INT REAL STRING CHAR VAR ARG
%token INTPTR CHARPTR REALPTR
%token IF ELSE DO WHILE FOR FUNC VOID RETURN 
%token AND OR EQ GTE LTE NOTEQ NOT
%token ID 
%token STR_VAL REAL_VAL CHAR_VAL NULLL DEC_VAL HEX_VAL BOOLVAL 


/*
%type <node> project program ret expr
%type <node> params parameters par value var_dec declars declartions statements
%type <node> proc procedures functype proctype body
*/
%start project
%%

project: cmt program {Printtree($2);}     
;

cmt: COMMENT cmt | 
;

program: processes {$$=mknode("CODE",
								$1,
								mknode(")",NULL,NULL);} 
;

processes : process processes {$$=mknode("",$1,$2);} 
			    			| {$$=NULL;}
;

process : FUNC ID "(" args ")" ":" prosSplit 

{$$=mknode("FUNC",
				mknode("PRINT_TOKEN",
						mknode($2,NULL,NULL),
						mknode("\n",$4,$7)),				
				mknode(")",NULL,NULL));}
;

prosSplit : type "{" bodyret "}" 
		
								{$$=mknode("\n",
												mknode("RET",
														mknode("PRINT_TOKEN",$1,NULL),
														mknode(")",NULL,NULL)),
												mknode("\n",
															mknode("BODY",
																		$3,
																		mknode(")",NULL,NULL))));} 
								
			| VOID "{" body "}"    

								{$$=mknode("\n",
											mknode("RET",
													mknode("PRINT_TOKEN",mknode("VOID",NULL,NULL),NULL),
													mknode(")",NULL,NULL)),
											mknode("\n",
														mknode("BODY",
																	$3,
																	mknode(")",NULL,NULL))));} 
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

argss: ";" arg argss {$$=mknode("",$2,NULL);}  
	      | {$$=NULL;}
;

arg : ARG ID ids ":" type  {$$=mknode("PRINT_TOKEN",
										$5,
										mknode("PRINT_TOKEN",
															mknode($2,$3,NULL),NULL));}
;

ids : ","ID ids {$$=mknode("PRINT_TOKEN",mknode($2,$3,NULL),NULL);}
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

funccall : ID "(" funcargs ")"";" 

{$$=mknode("CALL FUNC", 
					mknode("PRINT_TOKEN",
										mknode($1,mknode("\n",NULL,NULL),NULL),
										mknode("PARAMS",
						 								$3,
														mknode(")",NULL,NULL)));}
;

funcargs:ID moreargs 
						{$$=mknode("PRINT_TOKEN",mknode($1,NULL,NULL),$2);}
		   			|  {$$=NULL;}
;

moreargs: "," ID moreargs {$$=mknode("PRINT_TOKEN",mknode($1,NULL,NULL),$2);}
			| {$$=NULL;}
;

vardic: VAR ID varasign varsdic ":" type ";" 
{$$=mknode("DECLARATION",
						$6,
						mknode("=",
								 $3,
								 mknode($2,
								 			$4,
								 			mknode(")",NULL,NULL))));}
		
;

varsdic: "," ID varasign varsdic 
									{$$=mknode("=",
													$3,
													mknode($2,
																$4,
																mknode(")",NULL,NULL)));}
								| {$$=NULL;}
;

varasign: "=" ex {$$=mknode("",$2,NULL),NULL);}| {$$=NULL;}
;
      
ex: "("ex")" {$$=mknode("",$2,NULL,NULL)}| 
	val      {$$=mknode("PRINT_TOKEN",$1,NULL);}|
    ID       {$$=mknode("PRINT_TOKEN",mknode($1,NULL,NULL),NULL);}|
	funccall {$$=mknode("",$1,NULL);} |
    ex op ex 
				{$$=mknode("OPERATION",
										mknode("PRINT_TOKEN",
															$2,
															mknode("",$1,$3)),
										mknode(")",NULL,NULL) ;} 
;

op: "+" {$$=mknode("+",NULL,NULL);}|
    "-" {$$=mknode("-",NULL,NULL);}| 
    "*" {$$=mknode("*",NULL,NULL);}|
    "/" {$$=mknode("/",NULL,NULL);}
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
forlop: FOR "(" init ";" expression ";" update ")" "{"lopbody"}"
	{$$=mknode("FOR",
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
																			mknode($9,$10,NULL),
																			NULL))),
								mknode(")",NULL,NULL)),
					mknode(")",NULL,NULL));}
;

init: ID "=" intval {$$ = mknode("=",
									mknode("PRINT_TOKEN",
														mknode($3,NULL,NULL),
														mknode(")",NULL,NULL)),
									$1;}
;
intval: DEC_VAL {$$=mknode($1,NULL,NULL);} | 
		ID		 {$$=mknode($1,NULL,NULL);}
;
expression: "("expression")" lopexp     {$$=mknode("",$2,$3);} |
            notexp lopexp    			{$$=mknode("",$1,$3);} |
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

not : NOT {$$ = mknode($1)} | {$$=NULL;}
;

notexp: NOT splitnot {$$=mknode("PRINT_TOKEN",mknode($1,$2,NULL),NULL);}
;

splitnot: ex                {$$=mknode("",$1,NULL);}  | 
	 "(" expression ")" 	{$$=mknode("",$2,NULL);} 
;


lopexp: oper expression lopexp
											{$$=mknode("OPERATION",
																	mknode("",
																				$2,
																				mknode("",$1,$3)),
																	mknode(")",NULL,NULL) ;}
										|   {$$=NULL;}
;

oper: EQ    {$$=mknode($1,NULL,NULL);}|
      GTE   {$$=mknode($1,NULL,NULL);}|
      LTE   {$$=mknode($1,NULL,NULL);}|
      NOTEQ {$$=mknode($1,NULL,NULL);}|
      ">"   {$$=mknode($1,NULL,NULL);}|
      "<"   {$$=mknode($1,NULL,NULL);}|
	  AND   {$$=mknode($1,NULL,NULL);}| 
      OR    {$$=mknode($1,NULL,NULL);}
;



update:ID updatechage {$$=mknode("UPDATE",
										mknode("PRINT_TOKEN",$2,NULL),
										mknode(")",NULL,NULL));}
;

updatechage : "+""+"         {$$=mknode("+1",NULL,NULL);}		         |
	          "-""-"         {$$=mknode("-1",NULL,NULL);}		         |
	          "*""=" DEC_VAL {$$=mknode("*",mknode("PRINT_TOKEN",
			  													mknode($3,NULL,NULL),
																NULL),
											NULL);} |
	          "+""=" DEC_VAL {$$=mknode("+",mknode("PRINT_TOKEN",
			  													mknode($3,NULL,NULL),
																NULL),
											NULL);} |
	          "-""=" DEC_VAL {$$=mknode("-",mknode("PRINT_TOKEN",
			  													mknode($3,NULL,NULL),
																NULL),
											NULL);}
;

lopbody: body    {$$=mknode("",$1,NULL);}|
	 bodyret {$$=mknode("",$1,NULL);}
;

whilelop: WHILE "(" expression ")" "{"lopbody"}"
	{$$=mknode(""
		,mknode("EXPRESSION",mknode("",$3,NULL),NULL)
		,mknode("{",mknode("",$6),NULL));} 
;

dolop: DO "{" lopbody "}" WHILE "(" expression ")"
	{$$=mknode("{",
		mknode("",$3,NULL),
		mknode("EXPRESSION",mknode("",$7,NULL),NULL);}
;

ifstate: IF "(" expression ")" "{" lopbody "}" elstate
{$$=mknode("IF-ELSE",$3,mknode("{",mknode("",$6,$8)));}
;

elstate : "{"lopbody"}"  {$$=mknode("{",mknode("",$2,NULL),NULL);}
			|{$$={NULL;} 
;     
      

body: cmt bodysplit {$$=mknode("",$2,NULL);};

bodysplit: process      body  {$$=mknode("",$1,$2);}|
           ifstate      body  {$$=mknode("",$1,$2);}|
           vardic       body  {$$=mknode("",$1,$2);}|
           lop          body  {$$=mknode("",$1,$2);}|
      	   funccall     body  {$$=mknode("",$1,$2);}|
      	   "{" body "}" body  {$$=mknode("{",
      	   									mknode("",$2,$4),
											mknode(")",NULL,NULL));}
							| {$$=NULL;}
;

bodyret:body RETURN retval {$$=mknode("",
										$1,
										mknode("RETURN_VAL",
															$3,
															mknode(")",NULL,NULL));}
										

										
										;};

retval: ID         {$$=mknode("PRINT_TOKEN",mknode($1,NULL,NULL),NULL);} |
        funccall   {$$=mknode("",$1,NULL);} |
        val        {$$=mknode("PRINT_TOKEN",mknode($1,NULL,NULL),NULL);} |
        expression {$$=mknode("",$1,NULL);}
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

void printTabs(int n)
{
	int i;
	for(i=0;i<n/3;i++)
		printf(" ");
}

void printSpace(int num){
	printf("%s"," "*num);
}

void Printtree(node* tree)
{	
	int spaces = 0;
	
	if((strcmp(tree->token,"CODE") == 0 )        ||
	   (strcmp(tree->token, "FUNC") == 0)        ||
	   (strcmp(tree->token, "BODY") == 0)        ||
	   (strcmp(tree->token, "ARGS") == 0)        ||
	   (strcmp(tree->token, "{") == 0)           ||
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
		if(strcmp(tree->token, "{") == 0){

			printf("(BLOCK \n");
		}
		else if(strcmp(tree->token, "DECLARATION") == 0){

			printf("(%s %s\n",tree->token, tree->left->token);

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
	else if (strcmp(tree->token, "(") == 0){

		node *temp = tree->left

		printf("%s %s " , tree->token , temp->left->token);
	
		do{
			
			printf(temp->right->token);
			temp = temp->right->left;

		}while(temp != NULL)
		printf(")");
			
	}

	else if(strcmp(tree->token, "PARAMS") == 0){
		
		if(tree->left != NULL){
			
			printf("%s",spaces);
			spaces += 4;
			printf("(PARAMS ");
			node* temp = tree->left;
			while (temp != NULL){
				printf("%s ", temp->left->token);
				temp = temp->right;
			}
			printf(")");
		}
		
		
	}
	else  if (strcmp(tree->token, "RET") == 0){
		printf("(RET %s )",tree->left->left->token);
	}
	else if ((strcmp(tree->token,"STR_VAL") == 0) ||
		 (strcmp(tree->token,"REAL_VAL") == 0)||
		 (strcmp(tree->token,"CHAR_VAL") == 0)||
		 (strcmp(tree->token,"DEC_VAL") == 0) ||
		 (strcmp(tree->token,"HEX_VAL") == 0) ||
		 (strcmp(tree->token,"BOOLVAL") == 0))||
		 (strcmp(tree->token,"BOOL") == 0)     ||
		 (strcmp(tree->token,"INT") == 0)      ||
		 (strcmp(tree->token,"REAL") == 0)     ||
		 (strcmp(tree->token,"STRING") == 0)   ||
		 (strcmp(tree->token,"CHAR") == 0)     ||
		 (strcmp(tree->token,"INTPTR") == 0))  ||
		 (strcmp(tree->token,"CHARPTR") == 0)) ||
		 (strcmp(tree->token,"REALPTR") == 0))
	{
		printf("%s",tree->token);
	}
	
	else if (strcmp(tree->token,"RIGHTBARCKET")){
		printf(")\n");
	}
	else if (strcmp(tree->token,"VOID")){
		printf("VOID");
	}
	//printf("\nDEBUG: %s\n", tree->token);
	int flag = 4;
	//printTabs(1);
	if(strcmp(tree->token, "var") == 0)
	{
		printf("(DECLARE ");
		flag=2;
	}
	else if(strcmp(tree->token, "TYPE") == 0)
	{
		printf("(TYPE ");
		flag=2;
	}
	else if(strcmp(tree->token, "if") == 0)
	{
		printf("(IF\n");
		flag = 1;
	}
	else if(strcmp(tree->token, "while") == 0)
	{
		printf("(WHILE\n");
		flag = 1;
	}
	else if(strcmp(tree->token, "for") == 0)
	{
		printf("(FOR\n");
		flag = 1;
	}
	else if(strcmp(tree->token, "FUNCTION") == 0 ||strcmp(tree->token, "proc") == 0 ||strcmp(tree->token, "CODE") == 0||strcmp(tree->token, "Call func") == 0)
	{
		printf("(%s \n",tree->token);
		flag= 2;
	}
		else if(strcmp(tree->token, "ARGS") == 0)
	{
		printf("(ARGS ");
		if(tree->left)
		{
			flag = 2;
			printf("\n");

		}
		else{
			printf(" NONE)\n");
		}
	}
	else if(strcmp(tree->token, "IF-ELSE") == 0)
	{
		printf("(IF-ELSE\n");
		printlevel--;
		flag = 1;
	}
	else if(strcmp(tree->token, "RETURN") == 0)
	{
		printf("(RET ");
		flag = 2;
	}
	else if(strcmp(tree->token, "{") == 0)
	{
        printf("(BLOCK\n");
	}
	else if(strcmp(tree->token, "}") == 0)
	{
		printf(")\n");
	}
	else if(strcmp(tree->token, "") == 0);
	else if(strcmp(tree->token, "(") == 0)
			printf("(");
	else if(strcmp(tree->token, "\n") == 0)
			printf("\n");
	else if(strcmp(tree->token, ")") == 0)
			printf(")\n");
	else if(strcmp(tree->token, ",") == 0)
			printf(",");
	else if(strcmp(tree->token, ";") == 0)
			printf("\n");
	else if(strcmp(tree->token, "&&") == 0 ||
strcmp(tree->token, "/") == 0 ||
strcmp(tree->token, "=") == 0 ||
strcmp(tree->token, "==") == 0 ||
strcmp(tree->token, ">") == 0 ||
strcmp(tree->token, ">=") == 0 ||
strcmp(tree->token, "<") == 0 ||
strcmp(tree->token, "<=") == 0 ||
strcmp(tree->token, "-") == 0 ||
strcmp(tree->token, "!") == 0 ||
strcmp(tree->token, "!=") == 0 ||
strcmp(tree->token, "||") == 0 ||
strcmp(tree->token, "+") == 0 ||
strcmp(tree->token, "*") == 0 ||
strcmp(tree->token, "&") == 0 ||
strcmp(tree->token, "^") == 0 ||
strcmp(tree->token, "|") == 0 ||
strcmp(tree->token, ",") == 0 )
	{
			printf("(%s",tree->token);
			flag=1;
			if(strcmp(tree->token, "=") == 0)
				flag=2;

	}
	else
	{
		if((tree && !tree->left && !tree->right)
		||strcmp(tree->token, "Main") == 0)
		{
			printf("%s ", tree->token);
		}
		else
		{
			printlevel++;
			printf("%s", tree->token);
			printlevel--;

		}
	}
	if (tree->left)
	{
		printlevel++;
		Printtree(tree->left);
		printlevel--;
	}

	if (tree->right)
	{
		printlevel++;
		Printtree(tree->right);
		printlevel--;

	}
	if(flag == 2)
		printf(")\n");

	if(flag == 1)
		printf(")");
	if(flag == 0)
		printf("\n)");
}

int yyerror(char *e)
{
	int yydebug=1;
	fflush(stdout);
	fprintf(stderr,"Error %s at line %d\n" ,e,yylineno);
	fprintf(stderr, "does not accept '%s'\n",yytext);

	return 0;
} */
