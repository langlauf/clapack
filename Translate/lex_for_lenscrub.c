# include "stdio.h"
# define U(x) ((x)&0377)
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX 200
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern unsigned char yytext[];
int yymorfg;
extern unsigned char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
         case 2:
        case 3:
        case 4:
         case 5:
         case 6:
        case 7:
        { printf("%s", yytext); /* unchanged */ }
break;
case 8:
        case 9:
case 10:
{ 
                          register int c, paran_count = 1;
                          printf("%s", yytext); /* unchanged */ 	
			  /* Loop until the correct closing paranthesis */
                          while (paran_count != 0) {
                              c = input();
                              if (c == '(') ++paran_count;
                              else if (c == ')') --paran_count;
                              putchar(c);
                          } 
                        }
break;
case 11:
{ ; /* omit */ }
break;
case 12:
{ ; /* omit -- f2c -A */ }
break;
case 13:
{ ; /* omit -- f2c without -A or -C++ */ }
break;
case 14:
	{ ; }
break;
case 15:
		{ printf("%s", yytext); /* unchanged */ }
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

15,
0,

15,
0,

15,
0,

15,
0,

15,
0,

15,
0,

15,
0,

15,
0,

11,
0,

14,
0,

9,
0,

12,
0,

5,
0,

1,
0,

4,
0,

10,
0,

6,
0,

7,
0,

8,
0,

2,
0,

3,
0,

12,
14,
0,

12,
0,

13,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,3,	1,0,	
2,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	4,11,	4,11,	
0,0,	0,0,	0,0,	0,0,	
107,108,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,4,	2,4,	1,5,	
0,0,	1,3,	1,3,	5,15,	
0,0,	4,11,	0,0,	0,0,	
0,0,	73,84,	0,0,	0,0,	
1,3,	0,0,	0,0,	0,0,	
0,0,	51,64,	54,67,	56,69,	
57,70,	65,76,	4,12,	4,12,	
4,12,	4,12,	4,12,	4,12,	
4,12,	4,12,	4,12,	66,77,	
12,12,	12,12,	12,12,	12,12,	
12,12,	12,12,	12,12,	12,12,	
12,12,	12,12,	71,82,	72,83,	
75,87,	79,90,	105,107,	0,0,	
9,22,	19,29,	1,6,	21,31,	
37,50,	1,7,	2,7,	30,41,	
2,10,	31,42,	1,8,	2,8,	
12,24,	16,21,	5,16,	18,28,	
7,19,	20,30,	25,35,	17,27,	
1,9,	2,9,	4,13,	4,13,	
4,13,	4,13,	4,13,	4,14,	
4,13,	4,13,	4,13,	4,13,	
4,13,	4,13,	4,13,	4,13,	
4,13,	4,13,	4,13,	4,13,	
4,13,	4,13,	4,13,	4,13,	
4,13,	4,13,	4,13,	4,13,	
6,17,	8,20,	10,23,	14,26,	
6,18,	22,32,	23,28,	26,36,	
27,37,	28,38,	29,39,	23,34,	
33,46,	34,47,	8,21,	35,48,	
29,40,	36,49,	8,17,	39,52,	
40,53,	22,33,	8,18,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	32,43,	41,54,	42,55,	
43,56,	44,57,	45,58,	46,59,	
47,60,	48,61,	49,62,	52,65,	
53,66,	32,44,	55,68,	32,45,	
58,71,	59,72,	60,73,	38,38,	
38,38,	38,38,	38,38,	38,38,	
38,38,	38,38,	38,38,	38,38,	
38,38,	62,74,	63,75,	68,79,	
95,98,	96,99,	97,100,	98,101,	
99,102,	100,103,	13,25,	101,104,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	38,51,	102,105,	
38,38,	38,38,	38,38,	38,38,	
38,38,	38,38,	38,38,	38,38,	
38,38,	38,38,	38,38,	38,38,	
38,38,	38,38,	38,38,	38,38,	
38,38,	38,38,	38,38,	38,38,	
38,38,	38,38,	38,38,	38,38,	
38,38,	38,38,	50,63,	50,63,	
50,63,	50,63,	50,63,	50,63,	
50,63,	50,63,	50,63,	50,63,	
103,106,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
67,67,	0,0,	0,0,	0,0,	
69,69,	0,0,	0,0,	0,0,	
67,67,	67,67,	0,0,	0,0,	
69,69,	69,69,	0,0,	0,0,	
0,0,	0,0,	0,0,	50,63,	
50,63,	50,63,	50,63,	50,63,	
50,63,	50,63,	50,63,	50,63,	
50,63,	50,63,	50,63,	50,63,	
50,63,	50,63,	50,63,	50,63,	
50,63,	50,63,	50,63,	50,63,	
50,63,	50,63,	50,63,	50,63,	
50,63,	67,67,	70,70,	67,67,	
67,67,	69,69,	0,0,	69,69,	
69,69,	0,0,	70,70,	70,70,	
74,85,	74,85,	67,78,	0,0,	
0,0,	0,0,	69,80,	93,93,	
93,93,	93,93,	93,93,	93,93,	
93,93,	93,93,	93,93,	93,93,	
93,93,	0,0,	76,76,	0,0,	
0,0,	0,0,	77,77,	74,85,	
0,0,	0,0,	76,76,	76,76,	
0,0,	0,0,	77,77,	77,77,	
0,0,	0,0,	0,0,	70,70,	
0,0,	70,70,	70,70,	0,0,	
67,67,	0,0,	0,0,	0,0,	
69,69,	0,0,	0,0,	0,0,	
70,81,	0,0,	0,0,	0,0,	
0,0,	0,0,	93,96,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	76,76,	
0,0,	76,76,	76,76,	77,77,	
0,0,	77,77,	77,77,	0,0,	
0,0,	0,0,	82,82,	0,0,	
76,88,	0,0,	0,0,	0,0,	
77,89,	0,0,	82,82,	82,82,	
0,0,	0,0,	70,70,	0,0,	
74,86,	74,86,	74,86,	74,86,	
74,86,	74,86,	74,86,	74,86,	
74,86,	74,86,	74,86,	74,86,	
74,86,	74,86,	74,86,	74,86,	
74,86,	74,86,	74,86,	74,86,	
74,86,	74,86,	74,86,	74,86,	
74,86,	74,86,	76,76,	83,83,	
0,0,	0,0,	77,77,	82,82,	
0,0,	82,82,	82,82,	83,83,	
83,83,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
82,91,	84,93,	84,93,	84,93,	
84,93,	84,93,	84,93,	84,93,	
84,93,	84,93,	84,93,	84,93,	
84,93,	84,93,	84,93,	84,93,	
84,93,	84,93,	84,93,	84,93,	
84,93,	84,93,	84,93,	84,93,	
84,93,	84,93,	84,93,	0,0,	
83,83,	0,0,	83,83,	83,83,	
85,85,	85,85,	0,0,	0,0,	
0,0,	0,0,	82,82,	0,0,	
0,0,	83,92,	86,86,	86,86,	
86,86,	86,86,	86,86,	86,86,	
86,86,	86,86,	86,86,	86,86,	
0,0,	0,0,	0,0,	85,85,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	83,83,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	86,95,	0,0,	86,86,	
86,86,	86,86,	86,86,	86,86,	
86,86,	86,86,	86,86,	86,86,	
86,86,	86,86,	86,86,	86,86,	
86,86,	86,86,	86,86,	86,86,	
86,86,	86,86,	86,86,	86,86,	
86,86,	86,86,	86,86,	86,86,	
86,86,	0,0,	0,0,	0,0,	
85,94,	85,94,	85,94,	85,94,	
85,94,	85,94,	85,94,	85,94,	
85,94,	85,94,	85,94,	85,94,	
85,94,	85,94,	85,94,	85,94,	
85,94,	85,94,	85,94,	85,94,	
85,94,	85,94,	85,94,	85,94,	
85,94,	85,94,	94,94,	94,94,	
94,94,	94,94,	94,94,	94,94,	
94,94,	94,94,	94,94,	94,94,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	94,97,	0,0,	94,94,	
94,94,	94,94,	94,94,	94,94,	
94,94,	94,94,	94,94,	94,94,	
94,94,	94,94,	94,94,	94,94,	
94,94,	94,94,	94,94,	94,94,	
94,94,	94,94,	94,94,	94,94,	
94,94,	94,94,	94,94,	94,94,	
94,94,	0,0,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-2,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+21,	0,		yyvstop+3,
yycrank+5,	0,		yyvstop+5,
yycrank+32,	0,		yyvstop+7,
yycrank+1,	yysvec+6,	yyvstop+9,
yycrank+50,	0,		yyvstop+11,
yycrank+1,	yysvec+6,	yyvstop+13,
yycrank+30,	yysvec+6,	yyvstop+15,
yycrank+0,	yysvec+4,	0,	
yycrank+32,	0,		0,	
yycrank+119,	0,		0,	
yycrank+31,	yysvec+13,	0,	
yycrank+0,	yysvec+5,	0,	
yycrank+1,	0,		0,	
yycrank+1,	0,		0,	
yycrank+6,	0,		0,	
yycrank+2,	0,		0,	
yycrank+5,	0,		0,	
yycrank+2,	0,		0,	
yycrank+50,	0,		0,	
yycrank+45,	0,		0,	
yycrank+0,	0,		yyvstop+17,
yycrank+6,	0,		0,	
yycrank+41,	yysvec+13,	0,	
yycrank+36,	0,		0,	
yycrank+44,	0,		0,	
yycrank+52,	0,		0,	
yycrank+2,	0,		0,	
yycrank+4,	0,		0,	
yycrank+80,	0,		0,	
yycrank+40,	0,		0,	
yycrank+49,	0,		0,	
yycrank+58,	0,		0,	
yycrank+53,	yysvec+13,	0,	
yycrank+2,	0,		0,	
yycrank+147,	0,		0,	
yycrank+58,	0,		0,	
yycrank+59,	0,		0,	
yycrank+68,	0,		0,	
yycrank+69,	0,		0,	
yycrank+64,	0,		0,	
yycrank+69,	0,		0,	
yycrank+70,	0,		0,	
yycrank+72,	0,		0,	
yycrank+83,	0,		0,	
yycrank+75,	0,		0,	
yycrank+85,	yysvec+13,	0,	
yycrank+222,	0,		0,	
yycrank+25,	0,		0,	
yycrank+76,	0,		0,	
yycrank+77,	0,		0,	
yycrank+26,	0,		0,	
yycrank+72,	0,		0,	
yycrank+27,	0,		0,	
yycrank+28,	0,		0,	
yycrank+71,	0,		0,	
yycrank+81,	0,		0,	
yycrank+84,	0,		0,	
yycrank+0,	0,		yyvstop+19,
yycrank+95,	yysvec+13,	0,	
yycrank+111,	0,		0,	
yycrank+0,	0,		yyvstop+21,
yycrank+29,	0,		0,	
yycrank+39,	0,		0,	
yycrank+-299,	0,		0,	
yycrank+112,	0,		0,	
yycrank+-303,	0,		0,	
yycrank+-345,	0,		0,	
yycrank+50,	0,		0,	
yycrank+51,	0,		0,	
yycrank+25,	0,		0,	
yycrank+347,	yysvec+13,	yyvstop+23,
yycrank+52,	0,		0,	
yycrank+-373,	0,		0,	
yycrank+-377,	0,		0,	
yycrank+0,	0,		yyvstop+25,
yycrank+53,	0,		0,	
yycrank+0,	0,		yyvstop+27,
yycrank+0,	0,		yyvstop+29,
yycrank+-429,	0,		0,	
yycrank+-470,	0,		0,	
yycrank+392,	0,		0,	
yycrank+511,	0,		0,	
yycrank+482,	0,		0,	
yycrank+0,	0,		yyvstop+31,
yycrank+0,	0,		yyvstop+33,
yycrank+0,	0,		yyvstop+35,
yycrank+0,	0,		yyvstop+37,
yycrank+0,	0,		yyvstop+39,
yycrank+0,	0,		yyvstop+41,
yycrank+315,	yysvec+84,	0,	
yycrank+586,	0,		0,	
yycrank+100,	0,		0,	
yycrank+101,	0,		0,	
yycrank+102,	0,		0,	
yycrank+110,	0,		0,	
yycrank+111,	0,		0,	
yycrank+112,	0,		0,	
yycrank+105,	0,		0,	
yycrank+133,	0,		0,	
yycrank+170,	0,		0,	
yycrank+0,	0,		yyvstop+43,
yycrank+35,	0,		0,	
yycrank+0,	0,		yyvstop+46,
yycrank+26,	0,		0,	
yycrank+0,	0,		yyvstop+48,
0,	0,	0};
struct yywork *yytop = yycrank+708;
struct yysvf *yybgin = yysvec+1;
unsigned char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,'.' ,01  ,
'0' ,'1' ,'1' ,'1' ,'1' ,'1' ,'1' ,'1' ,
'1' ,'1' ,01  ,';' ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'a' ,'a' ,'a' ,'a' ,'a' ,'a' ,'a' ,
'a' ,'a' ,'a' ,'a' ,'a' ,'a' ,'a' ,'a' ,
'a' ,'a' ,'a' ,'a' ,'a' ,'a' ,'a' ,'a' ,
'a' ,'a' ,'a' ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
0};
unsigned char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*
 * (c) Copyright 1990, OPEN SOFTWARE FOUNDATION, INC.
 * ALL RIGHTS RESERVED
 */
/*
 * OSF/1 Release 1.0
*/
/*
#
# IBM CONFIDENTIAL
# Copyright International Business Machines Corp. 1989
# Unpublished Work
# All Rights Reserved
# Licensed Material - Property of IBM
#
#
# US Government Users Restricted Rights - Use, duplication or
# disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
# 
*/
/* @(#)ncform	1.3  com/lib/l,3.1,8951 9/7/89 18:48:47 */
int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
unsigned char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
unsigned char yysbuf[YYLMAX];
unsigned char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	unsigned char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( yyt > yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if(yyt < yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}
