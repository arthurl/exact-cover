#ifndef _dlx_h
#define _dlx_h

#include <stdlib.h>
#include <limits.h>

typedef unsigned int depth_t;
typedef int column_name;
#define COLUMN_NAME_MAX INT_MAX
#define COLUMN_NAME_MIN INT_MIN

/*
 * Column and data object
 */
struct cell {
  struct cell *L;
  struct cell *R;
  struct cell *U;
  struct cell *D;
  struct cell *C;
  size_t S;
  column_name N;
};

struct cell *create_empty_matrix();
void free_matrix(struct cell *h);
int add_set(struct cell **xp, column_name *set, size_t n_set_elem);
int dlx_solve(const struct cell *h, depth_t k, struct cell ***output_arr, size_t *output_arr_size);
int solve(const struct cell *h, size_t suggested_set_size, int ***set_covers, size_t **set_cover_sizes, size_t *n_sets);

#endif
