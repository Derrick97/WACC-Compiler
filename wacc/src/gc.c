/* A Mark-and-Sweep garbage collector.
   This cannot be even more textbookish.

   author: Yicheng Luo (yicheng.luo16@imperial.ac.uk)
   created: 1 Dec, 2017
*/
#include "stdlib.h"
#include "stdio.h"

#define UNMARKED 0
#define MARKED   1

typedef struct {
  struct node* first_node;
} gc_ctx;

gc_ctx* global_ctx;

void init_gc_ctx(void) {
  printf("[GC] creating GC context\n");
  global_ctx = malloc(sizeof(gc_ctx));
  global_ctx->first_node = NULL;
}

void destroy_gc_ctx(void) {
  printf("[GC] destroying GC context\n");
  free(global_ctx);
}

// Heap allocated data is always typed
enum ty {
  TY_INT,
  TY_PAIR
};

struct node {
  unsigned char marked;
  enum ty t;
  struct node* next; /* to form a linked list, we keep track of the next object */
  union {            /* keeps the payload of the node */
    int node_int;
    struct pair*  node_pair;
  };
};

struct pair {
  struct node* fst;
  struct node* snd;
};

struct array {
  struct data* arr;
};

typedef struct node node;

inline void unmark(node* n) {
  n->marked = UNMARKED;
}

inline void mark(node* n) {
  n->marked = MARKED;
}

void markAll(node* root) {
  mark(root);
  switch (root->t) {
  case TY_INT:
    return;
  case TY_PAIR:
    markAll(root->node_pair->fst); /* TODO no recursion */
    markAll(root->node_pair->snd);
    break;
  }
}

node* allocate(enum ty t) {
  node* ptr = malloc(sizeof(node));
  switch (t) {
  case TY_INT:
    break;
  case TY_PAIR:
    ptr->node_pair->fst = malloc(sizeof(node));
    ptr->node_pair->snd = malloc(sizeof(node));
    break;
  }
  ptr->marked = UNMARKED;

  /* update global linked list */
  ptr->next = global_ctx->first_node;
  global_ctx->first_node = ptr;
  return ptr;
}

void deallocate(node* node) {
  switch (node->t) {
  case TY_INT:
    return;
  case TY_PAIR:
    deallocate(node->node_pair->fst);
    deallocate(node->node_pair->snd);
    break;
  }
}

void sweepAll(void) {
  node* ptr = global_ctx->first_node;
  while (ptr != NULL) {
    if (!ptr->marked) {
      deallocate(ptr);
    }
    ptr = ptr->next;
  }
}

void gc(void) {
  printf("[GC] garbage collection occurred");
  /* TODO */
}
