#ifndef WACCLIB_H
#define WACCLIB_H

#include "stdio.h"
#include "stdlib.h"
#include "string.h"


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

int wacc_len(void* arr_ptr){
  // For arrays in WACC, we use the first int to store the length. subsequent storage is used
  // for the content of the array;
  return ((int*)arr_ptr)[0];
}

int wacc_print_array(char ptr) {
  printf("0x%x",  ptr);
  return 0;
}

int wacc_print_pair(void* ptr) {
  printf("0x%x", ptr);
  return 0;
}

int wacc_exit(char code) {
  exit(code);
}

int wacc_throw_overflow_error() {
  printf("OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n");
  exit(255);
}

int wacc_throw_division_by_zero() {
  printf("DivideByZeroError: division by zero.\n");
  exit(255);
}

int wacc_mod(int n, int d) {
  if (d == 0) {
    wacc_throw_division_by_zero();
  }
  return n % d;

}

int wacc_div(int a, int b) {
  if (b == 0) {
    wacc_throw_division_by_zero();
  }
  return a / b;
}

#endif
