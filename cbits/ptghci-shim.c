#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define BUFFERSIZE 10

#include "ptghci-shim.h"

int run_python_interpreter() {
  printf("Hello there");
  FILE* pyMainFile = fopen("/home/richard/workspace/ptghci/main.py", "r");
	Py_SetProgramName(L"This is the name");
  wchar_t *argv[] = {L"python", L"/home/richard/workspace/ptghci/main.py"};
  return Py_Main(2, argv);
  Py_Initialize();
  return PyRun_InteractiveOne(pyMainFile, "main.py");
  Py_Finalize();
}

/*
int run_python_interpreter() {
  char *text = calloc(1,1), buffer[BUFFERSIZE];
  printf("Enter a message: \n");
  while( fgets(buffer, BUFFERSIZE , stdin) ) // break with ^D or ^Z
  {
    text = realloc( text, strlen(text)+1+strlen(buffer) );
    strcat( text, buffer ); // note a '\n' is appended here everytime 
    printf("%s\n", buffer);
  }
  printf("\ntext:\n%s",text);
  return 0;
}
*/
