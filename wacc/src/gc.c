/* A Mark-and-Sweep garbage collector.
 *  This cannot be even more textbookish.
 *
 *  author: Yicheng Luo (yicheng.luo16@imperial.ac.uk)
 *  created: 1 Dec, 2017
 */

#include "stdlib.h"
#include "stdint.h"
#include "stdio.h"
#include "assert.h"
#include "stdarg.h"
#include "string.h"

#ifdef DEBUG
#define log(...) \
  do { if (DEBUG) fprintf(stderr, __VA_ARGS__); } while (0)
#else
#define log(...) \
  do { } while (0)
#endif


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

/** node represents an allocated
    block on heap. node is typed (as represented by the t field),
    which means that our GC knows the strategy to collect a block (rather than
    trying to be conservative).
 */
struct node {
  unsigned char marked;
  enum ty t;
  struct node* next; /* to form a linked list, we keep track of the next object */
  /* keeps the payload of the node */
  union {
    int node_int;               /* a int allocated on heap */
    struct pair* node_pair;     /* a pair */
  };
};

typedef struct node node;


#define UNMARKED 0
#define MARKED   1
#define MAX_CAPACITY 2

/* setters and getters for the codegen to invoke in order
   to trigger the automatic memory management process
 */
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

void unmark(node* n) {
  n->marked = UNMARKED;
}

void mark(node* n) {
  n->marked = MARKED;
}


/* start from [root], recursively mark all objects from this root */
void mark_all(node* root) {
  log("[GC] marked node at 0x%x\n", (uintptr_t)root);
  if (root->marked) return; // avoid cyclic references
  root->marked = MARKED;
  switch (root->t) {
  case TY_INT:
    return;
  case TY_PAIR:
    mark_all(root->node_pair->fst);
    mark_all(root->node_pair->snd);
    break;
  }
}

node* allocate(enum ty t) {
  log("[GC] allocate ");
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
    log("[GC] collect int at 0x%x\n", (uintptr_t)node);
    break;
  case TY_PAIR:
    deallocate(node->node_pair->fst);
    deallocate(node->node_pair->snd);
    free(node->node_pair->fst);
    free(node->node_pair->snd);
    free(node->node_pair);
    log("[GC] collect pair at 0x%x\n", (uintptr_t)node);
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
    int collected = 0;
    if (ptr->marked) {
      ptr->marked = UNMARKED;
    } else {
      deallocate(ptr);
      prev->next = next;
      collected = 1;
    }
    ptr = next;

    if (collected && global_ctx->first_node == prev)
      global_ctx->first_node = ptr;

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

extern void finalize(void) {
  node* ptr = global_ctx->first_node;
  while (ptr != NULL) {
    ptr->marked = UNMARKED;
    deallocate(ptr);
    ptr = ptr->next;
  }
}

void destroy_gc_ctx(void) {
  finalize();
  free(global_ctx);
  log(("[GC] destroyed GC context\n"));
}
