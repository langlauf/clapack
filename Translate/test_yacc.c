
# line 2 "test_yacc.y"

/* ----------------------------------------------------------------------------------
   Description : Routine to process raw f2c translated files. This routine
   generates macros for matrices. It is used in conjunction with the lex
   routine test_tool.l
   ----------------------------------------------------------------------------------
   Author : George Levy                      Date : 28/1/1994
   ----------------------------------------------------------------------------------- */

#include <stdio.h>
#include <ctype.h>

char *extract_token (char *string, char *token);
char *global_substitute (char *input_string, char *old_token, char* new_token);

char local_buf[80];
char row_string[80];
char col_string[80];
int  i,j,k,l,m,n,second_pass;
char leading_buf[200];
char integer_buf[200];

FILE *file;

#define LEADING_EXPRESSION 1
#define NOT_LEADING_EXPRESSION 0

# line 31 "test_yacc.y"
typedef union  { 
  char buf[200]; 
} YYSTYPE;
# define OPEN_BRACKET 257
# define CLOSE_BRACKET 258
# define TIMES 259
# define PLUS 260
# define DIVIDE 261
# define MINUS 262
# define C_SHIFT 263
# define OTHER 264
# define END_MATRIX 265
# define LEADING_DIMENSION 266
# define INDEX_VARIABLE 267
# define FUNCTION_MACRO 268
# define OFFSET 269
# define DE_REFERENCE 270
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
typedef int yytabelem;
# define YYERRCODE 256

# line 266 "test_yacc.y"

         /* lexical analysis */

#include "test_tool.c"

/* ----------------------------------------------------------- */

create_macro_index(char *list, int is_leading)
{
 
   char token1[80];
   char token2[80];
   char token3[80], sub_token3[80];
   char *ptr;
   char *null = (char *)0;
   int i, j, temp;
  
   if (is_leading) {  /* create the column index */

     col_string[0] = '\0';

     i = 0;
     while(array_name[i] != '[' ) {
       token1[i] = array_name[i];
       ++i;
     }
     token1[i] = '\0';
     token2[0] = '*';
     token2[1] = '\0';
 
     if (strstr(list,"<<")) { 

       /* have shift operation in string */

       /* first deal with known /small powers of 2 */

       temp = 1;
       for (j=1; j < 10; ++j) {
         temp = temp*2;
         sprintf(token3,"%s_dim1<<%d",token1,j);
         sprintf(sub_token3,"%d",temp);

         if (strstr(list,token3)) {
           strcpy(col_string,global_substitute(list, token3, sub_token3));
           j = 30; /* exit loop because only have one _dim1 value */
         }

       }

       if(j < 11) {  /* then haven't already called global_substitute */

           sprintf(token3,"%s_dim1<<",token1);
           strcpy(col_string,global_substitute(list, token3, "1<<"));
       }
     }

     strcat(token2,token1);
     strcat(token1,"_dim1*");
     strcat(token2,"_dim1");

     if (strstr(list,token1)) {
        strcpy(col_string, extract_token(list,token1));
     }
     if (strstr(list,token2)) {
       strcpy(col_string, extract_token(list,token2));
     }


     /* get rid of brackets for the case (var) */

      if ((col_string[0] == '(') && (col_string[strlen(col_string)-1] == ')')) {
         col_string[strlen(col_string)-1] = '\0';
         ptr = col_string;
         ++ptr;
         strcpy(col_string,ptr);    
      }

    }

  else {  /* create the row index */

    row_string[0] = '\0';
    strcpy(row_string,list);    

  }

}

/* ----------------------------------------------------------- */

char *extract_token (char *string, char *token)

{

char temp_buf[100];
char *ptr;
char *ptr1;
int pos1, pos2, token_len, i, j;


  token_len = strlen(token);
  ptr = strstr(string,token);
  pos1 = (int)ptr - (int)string;

  for (i= 0; i < pos1; ++i)
    temp_buf[i] = string[i];

  pos2 = pos1 + token_len;
  j = pos1;
  for (i = pos2; i < strlen(string); ++i, ++j)
    temp_buf[j] = string[i];

  temp_buf[j] = '\0';

  return temp_buf;

}

/* ----------------------------------------------------------- */

output_macro ()

{  /* output the matrix element in macro form */

  int i;

  i = 0;  
  while (array_name[i] != '[') {
   printf("%c",toupper(array_name[i]));
   ++i;
  }

  /* If strlen(col_string) = 0 then have
     f2c indexing of the form a[k + a_dim1] so create macro A(k,1).
     Otherwise have full index expression using both 
     row_string and col_string.
  */

  if(!strlen(col_string)) 
     printf("(%s,1)",row_string);   
  else
     printf("(%s,%s)",row_string,col_string);   
  

}


/* ---------------------------------------------------------------------- */

char *global_substitute (char *input_string, char *old_token, char* new_token)

{  /* this routine globally substitutes new_token for old_token within the
      string input_string */



char res[100];
char *ptr1;
char *ptr2;
int pos1, pos2, token_len, i, j;
int s_len;

  res[0] = '\0'; 
  ptr1 =  input_string;
  ptr2 = strstr(input_string, old_token);
  token_len = strlen(old_token);

  while (ptr2) {

   s_len = ptr2 - ptr1;
   strncat(res, ptr1, s_len);
   strcat(res, new_token);
   ptr1 = ptr2 + token_len;
   ptr2 = strstr(ptr1, old_token);
  }

  strcat(res, ptr1);

  return res;

}

main(argc,argv)
int argc;
char *argv[]; 
{

  second_pass = 0;

  file = fopen(argv[1], "r");
  yyin = file;  

  yyparse();


}

int yywrap()
{

  /* Use yywrap to find out if there was a Parameter Adjustment Section
     - if not then will not have performed a second pass */
 
   /*
       printf("in yywrap second_pass is %d\n",second_pass);
       printf("in yywrap vec_index is %d\n",vec_index);
       printf("in yywrap mat_index is %d\n",mat_index);

    */

  if (!second_pass) {

     fseek(file,0L,SEEK_SET);
     not_param_adjustments = 1;
     second_pass = 1;
     return 0;

  }
  else {
    close(file);
    return 1;

  }


}







yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 30
# define YYLAST 122
yytabelem yyact[]={

    13,    27,    12,     5,    13,     4,    12,     8,     9,    11,
    14,    15,    10,    11,    14,    15,    38,     3,    37,    50,
    51,    49,    24,    23,    25,     2,    14,    15,    50,    22,
    49,    24,    23,    25,    48,    18,    47,    19,    17,    20,
    26,    27,    18,    47,    19,    17,    20,    22,    49,    24,
    23,    25,    22,    21,    24,    23,    25,    18,    16,    19,
    17,    20,    51,    22,    24,    24,    25,    25,    18,    33,
    19,     7,    20,     1,     0,     0,     0,     0,     0,     0,
     0,     0,    31,    29,     6,     0,    30,     0,    35,    36,
    39,    41,    43,    44,    45,    46,    28,     0,     0,     0,
    32,    34,     0,     0,    40,    42,     0,     0,    52,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    53,
     0,    54 };
yytabelem yypact[]={

 -1000,  -257, -1000, -1000, -1000, -1000,  -202,  -207, -1000, -1000,
 -1000, -1000,  -226,  -253, -1000, -1000,  -253,  -253,  -253,  -241,
  -241,  -253,  -253,  -241,  -241,  -241, -1000, -1000,  -224,  -230,
  -212,  -191,  -191,  -212, -1000, -1000, -1000,  -266,  -241, -1000,
  -217,  -196, -1000, -1000,  -197, -1000, -1000,  -253, -1000,  -241,
 -1000,  -241,  -239,  -197, -1000 };
yytabelem yypgo[]={

     0,    73,    82,    69,    25,    17,     5,     3 };
yytabelem yyr1[]={

     0,     1,     1,     1,     1,     1,     4,     4,     7,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     5,     6 };
yytabelem yyr2[]={

     0,     0,     4,     4,     4,     4,     7,     7,     3,     3,
     7,     5,     7,     7,     7,     7,     7,     7,     7,     3,
     3,     5,     7,     7,     7,     7,     7,     7,     3,     3 };
yytabelem yychk[]={

 -1000,    -1,    -4,    -5,    -6,    -7,    -2,    -3,   264,   265,
   269,   266,   259,   257,   267,   268,   260,   262,   259,   261,
   263,   260,   259,   262,   261,   263,   266,   267,    -2,    -3,
    -3,    -2,    -2,    -3,    -2,    -3,    -3,   259,   257,    -3,
    -2,    -3,    -2,    -3,    -3,    -3,    -3,   260,   258,   260,
   258,   259,    -3,    -3,    -3 };
yytabelem yydef[]={

     1,    -2,     2,     3,     4,     5,     0,     0,    28,    29,
     8,     9,     0,     0,    19,    20,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    11,    21,     0,     0,
     6,    10,    12,     0,    13,    15,    16,     0,     0,    17,
     7,    22,    14,    24,    23,    25,    26,     0,    18,     0,
    27,     0,     0,    22,    24 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"OPEN_BRACKET",	257,
	"CLOSE_BRACKET",	258,
	"TIMES",	259,
	"PLUS",	260,
	"DIVIDE",	261,
	"MINUS",	262,
	"C_SHIFT",	263,
	"OTHER",	264,
	"END_MATRIX",	265,
	"LEADING_DIMENSION",	266,
	"INDEX_VARIABLE",	267,
	"FUNCTION_MACRO",	268,
	"OFFSET",	269,
	"DE_REFERENCE",	270,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"c_statement : /* empty */",
	"c_statement : c_statement index_exp",
	"c_statement : c_statement other_exp",
	"c_statement : c_statement end_array",
	"c_statement : c_statement offset_expr",
	"index_exp : leading_exp PLUS integer_exp",
	"index_exp : integer_exp PLUS leading_exp",
	"offset_expr : OFFSET",
	"leading_exp : LEADING_DIMENSION",
	"leading_exp : leading_exp PLUS leading_exp",
	"leading_exp : TIMES LEADING_DIMENSION",
	"leading_exp : leading_exp MINUS leading_exp",
	"leading_exp : leading_exp TIMES leading_exp",
	"leading_exp : integer_exp TIMES leading_exp",
	"leading_exp : leading_exp TIMES integer_exp",
	"leading_exp : leading_exp DIVIDE integer_exp",
	"leading_exp : leading_exp C_SHIFT integer_exp",
	"leading_exp : OPEN_BRACKET leading_exp CLOSE_BRACKET",
	"integer_exp : INDEX_VARIABLE",
	"integer_exp : FUNCTION_MACRO",
	"integer_exp : TIMES INDEX_VARIABLE",
	"integer_exp : integer_exp PLUS integer_exp",
	"integer_exp : integer_exp MINUS integer_exp",
	"integer_exp : integer_exp TIMES integer_exp",
	"integer_exp : integer_exp DIVIDE integer_exp",
	"integer_exp : integer_exp C_SHIFT integer_exp",
	"integer_exp : OPEN_BRACKET integer_exp CLOSE_BRACKET",
	"other_exp : OTHER",
	"end_array : END_MATRIX",
};
#endif /* YYDEBUG */
/* 
 *	Copyright 1987 Silicon Graphics, Inc. - All Rights Reserved
 */

/* #ident	"@(#)yacc:yaccpar	1.10" */
#ident	"$Revision: 1.5 $"

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-1000)

/*
** global variables used by the parser
*/
YYSTYPE yyv[ YYMAXDEPTH ];	/* value stack */
int yys[ YYMAXDEPTH ];		/* state stack */

YYSTYPE *yypv;			/* top of value stack */
int *yyps;			/* top of state stack */

int yystate;			/* current state */
int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */
int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */



/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ YYMAXDEPTH ] )	/* room on stack? */
		{
			yyerror( "yacc stack overflow" );
			YYABORT;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 6:
# line 53 "test_yacc.y"
{
                create_macro_index(yypvt[-2].buf,LEADING_EXPRESSION);
                create_macro_index(yypvt[-0].buf,NOT_LEADING_EXPRESSION);
                output_macro();

	     } break;
case 7:
# line 60 "test_yacc.y"
{

                create_macro_index(yypvt[-0].buf,LEADING_EXPRESSION);
                create_macro_index(yypvt[-2].buf,NOT_LEADING_EXPRESSION);
                output_macro();
	     } break;
case 8:
# line 68 "test_yacc.y"
{
                /* -----------------------------------------------------
                   deal with any array index that contains offset in
                   a simplistic way : ie assume that always refers
                   to element (1,1). We therefore assume that all
                   expressions are of the form a[a_offset] -> A(1,1) 
                   in macro form.
                  ------------------------------------------------------ */
                  
                   /* change array_name to upper case and load into local_buf */

                   i = 0;
                   while (array_name[i] != '[') {
                      local_buf[i] = toupper(array_name[i]);
                      ++i;
                   }
   
                   local_buf[i] = '\0';
                   strcat(local_buf,"(1,1)");

                   printf("%s",local_buf);

              } break;
case 9:
# line 94 "test_yacc.y"
{ 
                   strcpy(yyval.buf,yylval.buf); 
                 } break;
case 10:
# line 98 "test_yacc.y"
{

                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,yypvt[-2].buf); 
                   strcat(yyval.buf,"+"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 11:
# line 108 "test_yacc.y"
{

                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,"*"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 12:
# line 116 "test_yacc.y"
{

                   yyval.buf[0] = '\0';

                   strcat(yyval.buf,yypvt[-2].buf);
                   strcat(yyval.buf,"-"); 
                   strcat(yyval.buf,yypvt[-0].buf); 
                  } break;
case 13:
# line 125 "test_yacc.y"
{

                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,yypvt[-2].buf); 
                   strcat(yyval.buf,"*"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 14:
# line 134 "test_yacc.y"
{

                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,yypvt[-2].buf); 
                   strcat(yyval.buf,"*"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 15:
# line 144 "test_yacc.y"
{
                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,yypvt[-2].buf); 
                   strcat(yyval.buf,"*"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 16:
# line 153 "test_yacc.y"
{
                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,yypvt[-2].buf); 
                   strcat(yyval.buf,"/"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 17:
# line 162 "test_yacc.y"
{

                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,yypvt[-2].buf); 
                   strcat(yyval.buf,"<<"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 18:
# line 172 "test_yacc.y"
{ 

                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,"("); 
                   strcat(yyval.buf,yypvt[-1].buf); 
                   strcat(yyval.buf,")"); 

		    } break;
case 19:
# line 182 "test_yacc.y"
{ 
                   strcpy(yyval.buf,yylval.buf); 
                 } break;
case 20:
# line 187 "test_yacc.y"
{ 
                   strcpy(yyval.buf,yylval.buf); 
                 } break;
case 21:
# line 192 "test_yacc.y"
{

                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,"*"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 22:
# line 200 "test_yacc.y"
{

                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,yypvt[-2].buf); 
                   strcat(yyval.buf,"+"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 23:
# line 209 "test_yacc.y"
{
                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,yypvt[-2].buf); 
                   strcat(yyval.buf,"-"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 24:
# line 218 "test_yacc.y"
{

                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,yypvt[-2].buf); 
                   strcat(yyval.buf,"*"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 25:
# line 228 "test_yacc.y"
{

                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,yypvt[-2].buf); 
                   strcat(yyval.buf,"/"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 26:
# line 238 "test_yacc.y"
{

                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,yypvt[-2].buf); 
                   strcat(yyval.buf,"<<"); 
                   strcat(yyval.buf,yypvt[-0].buf); 

                  } break;
case 27:
# line 248 "test_yacc.y"
{ 
                   yyval.buf[0] = '\0';
                   strcat(yyval.buf,"("); 
                   strcat(yyval.buf,yypvt[-1].buf); 
                   strcat(yyval.buf,")"); 

		    } break;
case 28:
# line 257 "test_yacc.y"
{
                     printf("%s",yylval.buf);  

                  } break;
case 29:
# line 263 "test_yacc.y"
{
                    ;
                  } break;
	}
	goto yystack;		/* reset registers in driver code */
}
