LOADLIBS = -ly -ll -lm  
LIBS = -ll -lm  
CFLAGS	= -O

all: lenscrub comment substitute_locals.exe test_tool.exe lint_locals.exe

lenscrub: lex_for_lenscrub.o
	cc -o lenscrub -O lex_for_lenscrub.o -ll
     
lex_for_lenscrub.o: lenscrub.l
	lex lenscrub.l
	mv -f lex.yy.c lex_for_lenscrub.c
	cc -c -O lex_for_lenscrub.c

comment: main.o lex_for_comment.o
	cc -o comment -O main.o lex_for_comment.o -ll

lex_for_comment.o: comment.l
	lex comment.l
	mv -f lex.yy.c lex_for_comment.c
	cc -c -O lex_for_comment.c

substitute_locals.exe: lex_for_substitute_locals.o
	cc lex_for_substitute_locals.o $(LIBS) -o substitute_locals.exe

test_tool.exe: test_yacc.o
	cc test_yacc.o $(LOADLIBS) -o test_tool.exe

lint_locals.exe: lex_for_lint.o
	cc lex_for_lint.o $(LIBS) -o lint_locals.exe

.c.o:
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o lenscrub comment *.exe
