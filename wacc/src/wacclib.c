#ifndef WACCLIB_H
#define WACCLIB_H

#include "stdio.h"
#include "stdlib.h"

char wacc_chr(int num) {
  return (char) num;
}

int wacc_ord(char c) {
  return (int) c;
}

char wacc_read_char() {
  char c;
  scanf("%c", &c);
  return c;
}

int wacc_read_int() {
  int n;
  scanf("%d", &n);
  return n;
}

int wacc_print_bool(char b) {
  if (b) {
    printf("true");
  } else {
    printf("false");
  }
  return 0;
}

int wacc_println(void) {
  printf("\n");
  return 0;
}

int wacc_print_char(char c) {
  printf("%c", c);
  return 0;
}

int wacc_print_string(char* s) {
  printf("%s", s);
  return 0;
}

int wacc_print_int(int num) {
  printf("%d", num);
  return 0;
}

int print_pair(char num) {
  printf("0x%x", num);
  return 0;
}


int wacc_print_array(char ptr) {
  printf("0x%x",  ptr);
  return 0;
}

int wacc_exit(char code) {
  exit(code);
}

#endif
