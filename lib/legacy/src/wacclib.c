#include "stdio.h"

char chr(int num) {
  return (char) num;
}

int ord(char c) {
  return (int) c;
}

char read_char() {
  char c;
  scanf("%c", &c);
  return c;
}

int read_int() {
  int n;
  scanf("%d", &n);
  return n;
}

int print_bool(char b) {
  if (b) {
    printf("true");
  } else {
    printf("false");
  }
  return 0;
}

int println_bool(char b) {
  print_bool(b);
  printf("\n");
  return 0;
}

int print_char(char c) {
  printf("%c", c);
  return 0;
}

int println_char(char c) {
  print_char(c);
  printf("\n");
  return 0;
}

int print_string(char* s) {
  printf("%s", s);
  return 0;
}

int println_string(char* s) {
  printf("%s", s);
  printf("\n");
  return 0;
}

int print_int(int num) {
  printf("%d", num);
  return 0;
}

int println_int(int num) {
  print_int(num);
  printf("\n");
  return 0;
}

int print_pair(char num) {
  printf("0x%x", num);
  return 0;
}

int println_pair(char num) {
  print_pair(num);
  printf("\n");
  return 0;
}

int print_array(char ptr) {
  printf("0x%x",  ptr);
  return 0;
}

int println_array(char ptr) {
  print_array(ptr);
  printf("\n");
  return 0;
}
