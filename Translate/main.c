#include <stdio.h>

FILE *sourceFile;
FILE *commentFile;
FILE *localVarFile;
FILE *codeFile;

extern int yylex();
main(int argc, char *argv[]){

  char fileName[30];

  strcpy(fileName, argv[argc-1]);
/*
  commentFile = fopen("commentFile", "w");
  localVarFile = fopen("localVarFile", "w");
  codeFile = fopen("codeFile", "w");
*/

  yylex();

/*
  fclose(commentFile);
  fclose(localVarFile);
  fclose(codeFile);
*/
}
