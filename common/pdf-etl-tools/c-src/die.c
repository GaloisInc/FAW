#include <stdio.h>

volatile char *p = 0;

int main( int argc, char** argv ) {
  printf("hello\n");
  printf("hello world\n");
  char c = *p;
}
