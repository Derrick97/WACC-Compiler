/* A Mark-and-Sweep garbage collector.
   This cannot be even more textbookish.

   author: Yicheng Luo (yicheng.luo16@imperial.ac.uk)
   created: 1 Dec, 2017
*/
#include "stdlib.h"
#include "stdint.h"
#include "stdio.h"
#include "assert.h"
#include "stdarg.h"

#define UNMARKED 0
#define MARKED   1
#define MAX_CAPACITY 2

#define DEBUG 1
#define log(...) \
  do { if (DEBUG) fprintf(stderr, __VA_ARGS__); } while (0)

typedef struct {
  struct node* first_node;
  int max_capacity;
} gc_ctx;

gc_ctx* global_ctx;

// Heap allocated data is always typed
enum ty {
  TY_INT = 0,
  TY_PAIR = 1
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
    struct pair* node_pair;
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
  log("[GC] Marked node at 0x%x\n", (uintptr_t)root);
  if (root->marked) return;        /* avoid cyclic references */
  root->marked = MARKED;
  switch (root->t) {
  case TY_INT:
    return;
  case TY_PAIR:
    mark_all(root->node_pair->fst); /* TODO no recursion */
    mark_all(root->node_pair->snd);
    break;
  }
}

node* pair_fst_get(node* p) {
  assert(p->t == TY_PAIR);
  return p->node_pair->fst;
}

node* pair_snd_get(node* p) {
  assert(p->t == TY_PAIR);
  return p->node_pair->snd;
}

void pair_fst_set(node* p, node* fst) {
  p->node_pair->fst = fst;
}

void pair_snd_set(node* p, node* snd) {
  p->node_pair->snd = snd;
}

int node_get_data(node* p) {
  switch (p->t) {
  case TY_INT:
    return (p->node_int);
  case TY_PAIR:
    return ((int)(p->node_pair));
  }
}

void node_set_data(node* p, void* d) {
  switch (p->t) {
  case TY_INT:
    p->node_int = ((int)d);
    break;
  case TY_PAIR:
    p->node_pair = ((struct pair*)d);
    break;
  }
}

node* allocate(enum ty t) {
  log("[GC] Allocate ");
  node* ptr = malloc(sizeof(node));
  switch (t) {
  case TY_INT:
    log("int at 0x%x\n", (uintptr_t)ptr);
    break;
  case TY_PAIR:
    log("pair at 0x%x\n", (uintptr_t)ptr);
    struct pair* p = malloc(sizeof(struct pair));
    ptr->node_pair = p;
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
  if (node->marked) {
    return;
  }
  switch (node->t) {
  case TY_INT:
    log("[GC] Deallocate int at 0x%x\n", (uintptr_t)node);
    break;
  case TY_PAIR:
    deallocate(node->node_pair->fst);
    deallocate(node->node_pair->snd);
    free(node->node_pair->fst);
    free(node->node_pair->snd);
    free(node->node_pair);
    log("[GC] Deallocate pair at 0x%x\n", (uintptr_t)node);
    break;
  }
  mark(node);
}

void sweep_all(void) {
  node* prev = global_ctx->first_node;
  if (prev == NULL) return;
  node* ptr = prev->next;

  while (ptr != NULL) {
    node* next = ptr->next;
    if (ptr->marked) {         /* not marked, hence unreachable */
      ptr->marked = UNMARKED;
    } else {
      deallocate(ptr);
      prev->next = next;
    }
    ptr = next;
    prev = ptr;
  }

  log("[GC] Swept\n");
}

void init_gc_ctx(void) {
  log("[GC] creating GC context\n");
  global_ctx = malloc(sizeof(gc_ctx));
  global_ctx->first_node = NULL;
  global_ctx->max_capacity = MAX_CAPACITY;
}

void destroy_gc_ctx(void) {
  log(("[GC] destroying GC context\n"));
  sweep_all();
  free(global_ctx);
}
