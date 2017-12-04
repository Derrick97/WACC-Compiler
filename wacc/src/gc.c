/* A Mark-and-Sweep garbage collector.
   This cannot be even more textbookish.

   author: Yicheng Luo (yicheng.luo16@imperial.ac.uk)
   created: 1 Dec, 2017
*/
#include "stdlib.h"
#include "stdio.h"

#define UNMARKED 0
#define MARKED   1
#define MAX_CAPACITY 2

typedef struct {
  struct node* first_node;
  int max_capacity;
} gc_ctx;

gc_ctx* global_ctx;

void init_gc_ctx(void) {
  printf("[GC] creating GC context\n");
  global_ctx = malloc(sizeof(gc_ctx));
  global_ctx->first_node = NULL;
  global_ctx->max_capacity = MAX_CAPACITY;
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

struct pair {
  struct node* fst;
  struct node* snd;
};

struct array {
  struct data* arr;
};

struct node {
  unsigned char marked;
  enum ty t;
  struct node* next; /* to form a linked list, we keep track of the next object */
  union {            /* keeps the payload of the node */
    int node_int;
    struct pair node_pair;
  };
};

typedef struct node node;

void unmark(node* n) {
  n->marked = UNMARKED;
}

void mark(node* n) {
  n->marked = MARKED;
}

void mark_all(node* root) {
  printf("Marked\n");
  if (root->marked) return;        /* avoid cyclic references */
  root->marked = MARKED;
  switch (root->t) {
  case TY_INT:
    return;
  case TY_PAIR:
    mark_all(root->node_pair.fst); /* TODO no recursion */
    mark_all(root->node_pair.snd);
    break;
  }
}

node* allocate(enum ty t) {
  fprintf(stderr, "allocate\n");
  node* ptr = malloc(sizeof(node));
  switch (t) {
  case TY_INT:
    break;
  case TY_PAIR:
    break;
  }
  ptr->t = t;
  ptr->marked = UNMARKED;
  /* update global linked list */
  ptr->next = global_ctx->first_node;
  global_ctx->first_node = ptr;
  return ptr;
}

void deallocate(node* node) {
  fprintf(stderr, "deallocate\n");
  switch (node->t) {
  case TY_INT:
    return;
  case TY_PAIR:
    deallocate(node->node_pair.fst);
    deallocate(node->node_pair.snd);
    break;
  }
  free(node);
}

void sweep_all(void) {
  printf("Sweeped\n");
  return;
  node* ptr = global_ctx->first_node;
  while (ptr != NULL) {
    node* next = ptr->next;
    if (!ptr->marked) {
      deallocate(ptr);
    } else {
      ptr->marked = UNMARKED;
    }
    ptr = next;
  }
}

void gc(node* root) {
  /* TODO */
  mark_all(root);
  sweep_all();
  printf("[GC] garbage collection occurred\n");
}

/* int main() { */
/*   init_gc_ctx(); */
/*   node* n_int0 = allocate(TY_INT); */
/*   node* n_int1 = allocate(TY_INT); */
/*   allocate(TY_INT); // Allocate garbage */
/*   node* n_pair = allocate(TY_PAIR); */
/*   n_pair->node_pair.fst = n_int0; */
/*   n_pair->node_pair.snd = n_int1; */
/*   gc(n_pair); */
/*   destroy_gc_ctx(); */
/* } */
